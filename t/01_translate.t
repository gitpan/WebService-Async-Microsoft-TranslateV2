use strict;
use warnings;
use IO::Prompt;
use Test::More;
use Data::Section::Simple qw(get_data_section);
use WebService::Async::Microsoft::TranslateV2;

# live test
my $app_id;
eval {
    local $SIG{ALRM} = sub { die "alarm\n" };
    alarm 30;
    prompt 'Do you want to run the live tests. Bing application id is necessary for these tests (y/N)?', -d => 'N';
    if ($_ =~ /\Ay(?:es)?/i) {
        prompt 'Enter your bing application id> ';
        $app_id = $_;
        chomp $app_id;
        if (!defined $app_id || $app_id eq '') {
            plan skip_all => 'You must get the correct bing application id.';
        }
        plan tests => 4;
    }
    else {
        plan skip_all => 'Test is canceled.';
    }
    alarm 0;
};

if (!defined $app_id) {
    plan skip_all => 'Test is canceled.';
}

my $translator = WebService::Async::Microsoft::TranslateV2->new(
    app_id => $app_id,
);
$translator->single_template(get_data_section('single'));
$translator->whole_template(get_data_section('whole'));
$translator->source_language('en');
$translator->set_destination_languages(qw(en));
$translator->set_message( message1 => 'test1' );
$translator->set_message( message2 => 'test2' );
$translator->set_message( message3 => 'test3' );
$translator->translate(
    on_each_translation => sub {
        my ($self, $ids, $res) = @_;
        my %hash = @{$ids};
        my $id = $hash{id};
        chomp $res;
        if ($id eq 'message1') {
            is $res, '<?xml version="1.0" encoding="UTF-8"?>
<result key="message1">
    <translated lang="en">test1</translated>
</result>';
        }
        elsif ($id eq 'message2') {
            is $res, '<?xml version="1.0" encoding="UTF-8"?>
<result key="message2">
    <translated lang="en">test2</translated>
</result>';
        }
        elsif ($id eq 'message3') {
            is $res, '<?xml version="1.0" encoding="UTF-8"?>
<result key="message3">
    <translated lang="en">test3</translated>
</result>';
        }
    },
    on_translation_complete => sub {
        my ($self, $all_res) = @_;
        chomp $all_res;
        like $all_res, qr{<[?]xml\sversion="1.0"\sencoding="UTF-8"[?]>\s+
<results>\s+
    <result\skey="message\d">\s+
        <translated\slang="en">test\d</translated>\s+
    </result>\s+
    <result\skey="message\d">\s+
        <translated\slang="en">test\d</translated>\s+
    </result>\s+
    <result\skey="message\d">\s+
        <translated\slang="en">test\d</translated>\s+
    </result>\s+
</results>}xms;
    },
);

__DATA__
@@ single
?= Text::MicroTemplate::encoded_string '<?xml version="1.0" encoding="UTF-8"?>'
? my ($id, $lang, $message) = @_
<result key="<?= $id ?>">
    <translated lang="<?= $lang ?>"><?= $message ?></translated>
</result>
@@ whole
?= Text::MicroTemplate::encoded_string '<?xml version="1.0" encoding="UTF-8"?>'
? my ($self, $messages) = @_;
<results>
? for my $key ($self->all_messages_ids) {
    <result key="<?= $key ?>">
?   for my $lang ($self->all_destination_languages) {
?       my $text = $self->get_translated_message(id => $key, language => $lang);
        <translated lang="<?= $lang ?>"><?= $text ?></translated>
?   }
    </result>
? }
</results>
