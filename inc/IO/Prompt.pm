#line 1
package IO::Prompt;

our $VERSION = '0.997001';

use strict;
use Carp;

use 5.008;
no warnings 'utf8';

our @EXPORT    = qw( prompt );
our @EXPORT_OK = qw( hand_print get_input );

use IO::Handle;
use Term::ReadKey;
use POSIX qw( isprint );

my $clearfirst;
my %input;

sub _clear {
    return unless $_[0];
    open my $OUT, ">/dev/tty" or croak "Cannot write to terminal: $!";
    print {$OUT} "\n" x 60;
    $clearfirst = 0;
}

our %flags_arg = (
    p  => 'prompt',
    s  => 'speed',
    e  => 'echo',
    r  => 'require',
    d  => 'default',
    u  => 'until',
    w  => 'while',
    nl => 'newline',
    m  => 'menu',
);

our %flags_alias = (
    '-okayif'  => '-while',   '-okay_if' => '-while',
    '-failif'  => '-until',   '-fail_if' => '-until',
);

our %flags_noarg = (
    y   => 'yes',
    n   => 'no',
    i   => 'integer',
    num => 'number',
    raw => 'raw_input',
    1   => 'onechar',
    c   => 'clear',
    f   => 'clearfirst',
    a   => 'argv',
    l   => 'line',
    t   => 'tty',
    x   => 'escape',
);

my $RECORD;    # store filehandle for __PROMPT__ file supporting -record flag

$flags_arg{$_}   = $_ for values %flags_arg;
$flags_noarg{$_} = $_ for values %flags_noarg;

my $flag_with_arg = join '|', reverse sort keys %flags_arg;
my $flag_no_arg   = join '|', reverse sort keys %flags_noarg;

my %yespat = (
    'y' => qr/^\s*[yY]/,
    'Y' => qr/^\s*Y/,
);

my %nopat = (
    'n' => qr/^\s*[nN]/,
    'N' => qr/^\s*N/,
);

my %num_pat = (
    integer => qr{[+-]? \d+ (?:[Ee]+?\d+ )?}x,
    number  => qr{[+-]? (?:\d+[.]?\d* | [.]\d+) (?:[Ee][+-]?\d+)? }x,
);

sub _get_prompt (\%@) {
    my ($flags, @data) = @_;
    my ($OUT);
    @data = map { $flags_alias{$_} || defined($_) ? $_ : "" } @data;
    for (my $i = 0 ; $i < @data ; $i++) {
        local *_ = \$data[$i];
        if (ref eq 'HASH') {
            splice @data, $i + 1, 0, %$_;
        }
        elsif (ref eq 'GLOB' or UNIVERSAL::isa($_, 'IO::Handle')) {
            croak "Can't write prompt to read-only $_" unless -w;
            $OUT = $_;
        }
        elsif (/^-/) {  # a flag
            s/_//g;
            if (s/^-(f|clearfirst)/-/) {
                $clearfirst = 1 unless defined $clearfirst;
            }
            elsif (s/^-(yes|y)/-/i) {
                $flags->{-yesno}{yes} = $yespat{ substr $1, 0, 1 };
                $flags->{-yesno}{yesprompt} = substr $1, 0, 1;
            }
            elsif (s/^-(?:nl|newline)/-/i) {
                $flags->{-nlstr} = $data[ $i + 1 ];
                undef $data[ $i++ ];
            }
            elsif (s/^-escape|-x/-/i) {
                $flags->{-escape} = 1;
            }
            elsif (s/^-raw_?(?:input)?/-/i) {
                $flags->{-raw_input} = 1;
            }
            elsif (s/^-number|-num/-/i) {
                $flags->{-number} = 'number';
            }
            elsif (s/^-integer|-i/-/i) {
                $flags->{-number} = 'integer';
            }
            elsif (s/^-(no|n)/-/i) {
                $flags->{-yesno}{no} = $nopat{ substr $1, 0, 1 };
                $flags->{-yesno}{noprompt} = substr $1, 0, 1;
            }
            elsif (m/^-($flag_with_arg)/) {
                croak "Missing argument for $_ option" if @data < $i+2 
                                                       || !defined $data[$i+1];
                s/^-($flag_with_arg)/-/;
                $flags->{ -$flags_arg{$1} } = $data[$i+1];
                undef $data[$i++];
            }
            elsif (s/^-($flag_no_arg)/-/) {
                $flags->{ -$flags_noarg{$1} } = 1;
            }
            else { croak "Unknown flag ($_) in prompt" }

            redo if defined $_ && /^-./;
        }
        else { next }
        undef $data[$i];
    }
    $_ =
        !defined() ? undef
      : ref eq 'Regexp' ? $_
      : qr/^\Q$_\E$/
      for @{$flags}{qw(-while -until -failif -okayif)};

    for (grep { defined } $flags->{ -require }) {
        croak "Argument to -require must be hash reference"
          unless ref eq 'HASH';
        my %reqs = %$_;
        $_ = sub {
            my ($input) = @_;
            for (keys %reqs) {
                return $_ unless _smartmatch($input, $reqs{$_});
            }
            return;
        };
    }
    my @prompt = grep { defined } @data;
    if (@prompt && exists $flags->{-default}) {
        my $prompt = join "", @prompt;
        $prompt =~ s/(:?\s*)$/ [$flags->{-default}]$1/ if $prompt !~ /\[.*\]/;
        @prompt = $prompt;
    }
    return $OUT, @prompt;
}

my $prompt_req = "(The value entered is not acceptable) ";

sub prompt {
    my $caller = caller;

    local $\ = q{};   # Make sure no funny business on print statements

    my %flags;
    my ($OUT, @prompt) = _get_prompt(%flags, @_);
    open $OUT, ">/dev/tty" or croak "Cannot write to terminal: $!" if !$OUT;
    $OUT->autoflush(1);
    @prompt = $flags{ -prompt } if !@prompt and $flags{ -prompt };
    my $IN;
    if ($flags{-tty} || $flags{-argv}) {
        open $IN, "</dev/tty" or croak "Cannot read from terminal: $!";
    }
    else {
        no strict 'refs';
        my $ARGV = $caller . "::ARGV";
        unless (*$ARGV->opened) {
            $$ARGV = shift(@$ARGV) || '-';
            open $ARGV or croak "Can't open $$ARGV: $!";
        }
        $IN = \*$ARGV;
        @prompt = () unless -t $IN;
    }
    $flags{-speed} = 0.075 unless defined $flags{-speed};
    use Want qw( want );
    $flags{-set_underscore} ||= want('BOOL');

    $clearfirst = 1 if !defined($clearfirst) && $flags{-clearfirst};
    _clear($flags{ -clear } || $clearfirst);
    my $input;
    if (-t $IN and defined $input{$caller}) {
        $input = _fake_from_DATA($caller, $IN, $OUT, \%flags, @prompt);
    }
    elsif ($flags{-argv}) {
        return if @ARGV;
        @prompt = "Args for $0: " if -t $IN and !@prompt;
        print {$OUT} @prompt;
        @ARGV = map glob, split /\s+/, get_input($IN, $OUT, \%flags, @prompt);
        return @ARGV;
    }
    elsif ($flags{-yesno}) {
        return _yesno($IN, $OUT, \%flags, @prompt);
    }
    elsif ($flags{-number}) {
        return _number($IN, $OUT, \%flags, @prompt);
    }
    elsif ($flags{-menu}) {
        return _menu($IN, $OUT, \%flags, @prompt);
    }
    else {
        print {$OUT} @prompt;
        $input = get_input($IN, $OUT, \%flags, @prompt);
    }
    return _tidy($input, %flags);
}

sub _tidy {
    my ($input, %flags) = @_;
    my $defined = defined $input;
    chomp $input if $defined && !$flags{-line};
    my $success = $defined
      && (!$flags{ -while } || $input =~ $flags{ -while })
      && (!$flags{ -until } || $input !~ $flags{ -until });
    print {$RECORD} $input, "\n" if $success && $RECORD;
    return "$input" if $flags{-raw_input};
    return bless {
        value   => $input,
        success => $success,
        set_val => $flags{ -set_underscore },
      },
      'IO::Prompt::ReturnVal';
}

sub _success {
    my ($val, $no_set, $raw) = @_;
    print {$RECORD} $val, "\n" if $val && $RECORD;
    return "$val" if $raw;
    return bless {
        value   => $val,
        success => 1,
        set_val => !$no_set,
      },
      'IO::Prompt::ReturnVal';
}

sub _failure {
    my ($val, $raw) = @_;
    return "$val" if $raw;
    return bless {
        value   => $val,
        success => 0,
        set_val => 0,
      },
      'IO::Prompt::ReturnVal';
}

sub import {
    my $class = shift;

    {
        no strict 'refs';
        *{ caller() . "::$_" } = \&{$_} for @EXPORT;

        foreach my $sym (@_) {
            grep { $_ eq $sym } @EXPORT_OK or next;
            *{ caller() . "::$sym" } = \&{$sym};
        }
    }

    @_ = grep /^-/, @_;
    $input{ caller() } = undef;
    if ("@_" eq "-clearfirst") {
        $clearfirst = 1;
        return;
    }
    for my $i (0 .. $#_) {
        last if $RECORD;
        if ($_[$i] eq '-record') {
            splice @_, $i, 1;
            open $RECORD, '>', '__PROMPT__'
              or croak "Can't open __PROMPT__ recording file: $!";
            print {$RECORD} "__DATA__\n__PROMPT__\n";
        }
    }
    prompt @_ if @_;
}

CHECK {
    for my $pkg (keys %input) {
        next if defined $input{$pkg};

        no strict 'refs';
        if (my $datahandle = *{"${pkg}::DATA"}{IO}) {
            local $/;
            my $data = <$datahandle>;
            if ($data =~ s/(\A|\n) __PROMPT__ \s*? \n (.*)/$1/xs) {
                $input{$pkg} = "$2";
            }
            else {
                delete $input{$pkg};
            }
            open "${pkg}::DATA", "<", \$data or die "Internal error: $!";
        }
        else {
            delete $input{$pkg};
        }
    }
}

my $baseline = ord 'A';

sub _visualize {
    local ($_) = @_;
    return isprint($_)         ? $_
         : $_ eq "\n"          ? $_
         : ord($_) == 0        ? ''
         : ord($_) < $baseline ? '^' . chr($baseline + ord($_) - 1)
         :                       '?'
}

sub hand_print {
    my $OUT   = \*STDOUT;
    my $echo  = undef;
    my $speed = 0.05;
    local $| = 1;
    for (@_) {
        if (ref eq 'HASH') {
            $speed = $_->{-speed} if exists $_->{-speed};
            $OUT   = $_->{-to}    if exists $_->{-to};
            $echo  = $_->{-echo}  if exists $_->{-echo};
        }
        elsif (!$speed) {
            print {$OUT} $_;
        }
        else {
            print {$OUT} $_ and select undef, undef, undef, rand $speed
              for map { defined $echo ? $echo : _visualize($_) } split "";
        }
    }
    return scalar @_;
}

sub _fake_from_DATA {
    my ($caller, $IN, $OUT, $flags, @prompt) = @_;
    local $SIG{INT} = sub { ReadMode 'restore', $IN; exit };
    ReadMode 'noecho', $IN;
    ReadMode 'raw',    $IN;
    print {$OUT} @prompt;
    my $input = getc $IN;
    if ($input =~ /\cD|\cZ/) { print {$OUT} _visualize($input),"\n"; return; }
    if ($input eq "\e") {
        ReadMode 'restore', $IN;
        return get_input($IN, $OUT, $flags, @prompt);
    }
    $input{$caller} =~ m/\G (?!\cD|\cZ) (.*) (\n?)/xgc;
    my ($line, $nlstr) = ($1, $2);
    unless (defined $line) {
        while ($input ne "\n") { $input = getc $IN }
        print {$OUT} "\n";
        return;
    }
    delete $input{$caller} if pos $input{$caller} == length $input{$caller};
    if ($input eq "\n") {
        hand_print { -to => $OUT, %$flags }, $line;
        unless (defined <$IN>) { print {$OUT} "\n"; return; }
    }
    else {
        my $i = 0;
        while (1) {
            my $done = $i >= length $line;
            print {$OUT} substr($line, $i++, 1) unless $done;
            if (getc $IN eq "\n") {
                last if $done;
                hand_print { -to => $OUT, %$flags }, substr($line, $i);
                $i = length $line;
            }
        }
    }
    ReadMode 'restore', $IN;
    print {$OUT} "\n";
    return $line . $nlstr;
}

sub get_input {
    my ($IN,      $OUT,   $flags, @prompt)  = @_;
    my ($onechar, $nlstr, $echo,  $require) =
      @{$flags}{ -onechar, -nlstr, -echo, -'require' };
    $nlstr = "\n" unless defined $nlstr;
    if (!-t $IN) {
        return scalar <$IN> unless $onechar;
        return getc $IN;
    }
    $OUT->autoflush(1);
    local $SIG{INT} = sub { ReadMode 'restore', $IN; exit };
    my ($input, $newlines);
    my %cntl = GetControlChars $IN;
    my $cntl = join '|', values %cntl;
    ReadMode 'raw', $IN;

  INPUT: while (1) {
        my $next = getc $IN;
        if ($next eq $cntl{INTERRUPT}) {
            ReadMode 'restore', $IN;
            exit;
        }
        elsif ($next eq $cntl{ERASE}) {
            if (defined $input && length $input) {
                substr($input, -1) = "";
                print {$OUT} "\b \b";
            }
            next;
        }
        elsif ($next eq $cntl{EOF}) {
            ReadMode 'restore', $IN;
            close $IN;
            return $input;
        }
        elsif ($flags->{-escape} && $next eq "\e") {
            ReadMode 'restore', $IN;
            print {$OUT} "<esc>";
            return "\e";
        }
        elsif ($next !~ /$cntl/ && defined $next) {
            $input .= $next;
            if ($next eq "\n") {
                if ($input eq "\n" && exists $flags->{-default}) {
                    print {$OUT} (
                         defined $echo
                         && $flags->{-menu} ? $echo
                       : defined $echo      ? $echo x length($flags->{-default})
                       :                      '['.$flags->{-default}.']'
                    );
                    print {$OUT} $nlstr;
                    ReadMode 'restore', $IN;
                    return $onechar ? substr($_, 0, 1) : $_
                      for $flags->{-default};
                }
                $newlines .= $nlstr;
            }
            else {
                print {$OUT}(defined $echo ? $echo : $next);
            }
        }
        else {
            $input .= $next;
        }
        if ($onechar or !defined $next or $input =~ m{\Q$/\E$}) {
            chomp $input unless $flags->{-line};
            if ($require and my $mesg = $require->($input)) {
                print {$OUT} "\r", " " x 79, "\r", sprintf($mesg, @prompt);
                undef $input;
                undef $newlines;
            }
            else {
                ReadMode 'restore', $IN;
                print {$OUT} $newlines if defined $newlines;
                return $onechar ? substr($input, 0, 1) : $input;
            }
        }
    }
}

sub _yesno {
    my ($IN,  $OUT, $flags,     @prompt)   = @_;
    my ($yes, $no,  $yesprompt, $noprompt) =
      @{ $flags->{ -yesno } }{qw(yes no yesprompt noprompt)};
    $yes = qr/^([^Nn])/ unless defined $yes;
    $no  = qr/^([^Yy])/ unless defined $no;
    my $prompt2 =
        $yesprompt && $noprompt ? "'$yesprompt' or '$noprompt'"
      : $yesprompt ? "'$yesprompt' for yes"
      : "'$noprompt' for no";
    my $raw = $flags->{-raw_input};
    print {$OUT} @prompt if -t $IN;
    while (1) {
        my $response =
          get_input($IN, $OUT, { %$flags, -nlstr => "" }, @prompt);
        chomp $response unless $flags->{-line};
        print {$OUT} "\n" and return _success($response, 'no_set', $raw)
            if defined $response and $response =~ /$yes/;
        print {$OUT} "\n" and return _failure($response, $raw)
          if !defined $response or $response =~ /$no/;
        print {$OUT} "\r", " " x 79, "\r", @prompt,
          "(Please answer $prompt2) "
          if -t $IN;
    }
}

sub _number {
    my ($IN, $OUT, $flags, @prompt) = @_;
    my $numtype    = $flags->{ -number };
    my $prompt_num = "(Please enter a valid $numtype) ";
    my $match      = $num_pat{$numtype};
    my $require    = $flags->{ -require };
    print {$OUT} @prompt if -t $IN;
    while (1) {
        my $response =
          get_input($IN, $OUT, { %$flags, -nlstr => "", -require => undef },
            @prompt);
        chomp $response if defined $response && !$flags->{-line};
        if (-t $IN and defined $response) {
            if ($response !~ /\A \s* $match \s* \Z/x) {
                print {$OUT} "\r", " " x 79, "\r", @prompt, $prompt_num;
                next;
            }
            elsif ($require and my $mesg = $require->($response)) {
                print {$OUT} "\r", " " x 79, "\r", sprintf($mesg, @prompt);
                next;
            }
        }
        print {$OUT} "\n" and return _tidy($response);
    }
}

sub _self { $_[0] }

sub _menu {
    my ($IN, $OUT, $flags, @prompt) = @_;
    my $datatype   = ref $flags->{ -menu };
    my @data = $datatype eq 'ARRAY'  ? @{ $flags->{ -menu } }
             : $datatype eq 'HASH'   ? sort keys %{ $flags->{ -menu } }
             : croak "Argument to -menu must be hash or array reference";

    my $val_for = $datatype eq 'ARRAY' 
                    ? \&_self
                    : sub { $flags->{ -menu }{$_[0]} };

    my $count = @data;

    croak "Too many -menu items" if $count > 26;
    croak "Too few -menu items"  if $count < 1;

    my $max_char = chr(ord('a') + $count - 1);
    my $menu = q{};

    my $default_key;
    my $next = 'a';
    for (@data) {
        my $item = $_;
        if (defined $flags->{ -default } && !defined $default_key && $item eq $flags->{ -default }) {
            $default_key = $next;
        }
        $item =~ s/\A/qq{     }.$next++.q{. }/xmse;
        $item =~ s/\n?\z/\n/xms;
        $item =~ s/(?!\Z)\n/\n        /gxms;
        $menu .= $item;
    }

    push @prompt, "\n$menu\n> ";

    my $prompt_range = "(Please enter a-$max_char) > ";
    my $require    = $flags->{ -require };
    print {$OUT} @prompt if -t $IN;
    while (1) {
        my $response =
          get_input($IN, $OUT, { %$flags, -escape => 1, -nlstr => "", -require => undef },
                    @prompt);
        chomp $response;
        if (-t $IN and defined $response) {
            if (length $response == 1 && $response eq "\e") {
                return $response;
            }
            elsif (length $response > 1 || ($response lt 'a' || $response gt $max_char) ) {
                if ($response ne $flags->{-default}) {
                    print {$OUT} "\r", " " x 79, "\r", $prompt_range;
                    next;
                }
                $response = $default_key;
            }
            elsif ($require and my $mesg = $require->($data[ord($response)-ord('a')])) {
                print {$OUT} "\r", " " x 79, "\r", sprintf($mesg, @prompt);
                next;
            }
        }
        print {$OUT} "\n";
        my $selection = $data[ord($response)-ord('a')];
        $response = defined $response ? $val_for->($selection) : $response;
        if (defined $response && ref($response) =~ m/\A(?:HASH|ARRAY)\z/xms ) {
            $response = _menu($IN, $OUT, {%{$flags}, -menu=>$response}, "$selection: ");
            if (defined $response && $response eq "\e") {
                print {$OUT} "\n", @prompt if -t $IN;
                next;
            }
        }
        return _tidy($response);
    }
}

sub _smartmatch {
    my ($str, $matcher) = @_;
    my $type = ref $matcher;
    my $res = $type eq 'CODE'
      ? do { local $_ = $str; $matcher->() }
      : $type eq 'Regexp' ? ($str =~ $matcher)
      : $type eq 'ARRAY' ? scalar grep({ _smartmatch($str, $_) } @$matcher)
      : $type eq 'HASH' ? $matcher->{$str}
      : $str eq $matcher;
    return $res;
}

package IO::Prompt::ReturnVal;

use overload
    q{bool} => sub {
        $_ = $_[0]{value} if $_[0]{set_val};
        $_[0]{handled} = 1;
        $_[0]{success};
    },
    q{""} => sub { $_[0]{handled} = 1; "$_[0]{value}"; },
    q{0+} => sub { $_[0]{handled} = 1; 0 + $_[0]{value}; },
    fallback => 1,
    ;

sub DESTROY {
    $_ = $_[0]{value} unless $_[0]{handled};
}

1; # Magic true value required at end of module
__END__

