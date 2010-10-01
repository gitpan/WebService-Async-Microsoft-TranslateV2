#line 1
package WebService::Async;
use Moose;
use Moose::Util::TypeConstraints;

use 5.008008;
our $VERSION = '0.01';

use constant MAX_MAX_PER_HOST        => 6;
use constant DEFAULT_MAX_PER_HOST    => 4;
use constant DEFAULT_TIMEOUT         => 10;
use constant DEFAULT_RETRY_INTERVAL  => 10;
use constant DEFAULT_MAX_RETRY_COUNT => 3;
use constant SUCCESS => 1;
use constant FAILURE => 0;

subtype 'WebService::Async::MaxPerHost' => as 'Int' =>
  where { $_ <= MAX_MAX_PER_HOST && $_ > 0 };

has base_url => (
    is  => 'rw',
    isa => 'Str',
);

has param => (
    is  => 'rw',
    isa => 'HashRef',
);

has data_uuid => (
    is         => 'bare',
    isa        => 'Data::UUID',
    lazy_build => 1,
    accessor   => '_data_uuid',
);

has auto_block => (
    is      => 'rw',
    isa     => 'Bool',
    default => 1,
);

has request_condvar => (
    is       => 'bare',
    isa      => 'AnyEvent::CondVar',
    accessor => '_request_condvar',
    builder  => '_build_request_condvar',
);

has response_cache => (
    is  => 'rw',
    isa => 'WebService::Async::ResponseCache',
);

has is_busy => (
    is       => 'bare',
    isa      => 'Bool',
    accessor => '_is_busy',
);

has max_per_host => (
    is  => 'rw',
    isa => 'WebService::Async::MaxPerHost',
    trigger =>
      sub { $AnyEvent::HTTP::MAX_PER_HOST = $_[1] || DEFAULT_MAX_PER_HOST; },
);

has timeout => (
    is      => 'rw',
    isa     => 'Int',
    default => DEFAULT_TIMEOUT,
);

has retry_interval => (
    is      => 'rw',
    isa     => 'Int',
    default => DEFAULT_RETRY_INTERVAL,
);

has max_retry_count => (
    is      => 'rw',
    isa     => 'Int',
    default => DEFAULT_MAX_RETRY_COUNT,
);

has request_queue => (
    traits   => ['Array'],
    is       => 'bare',
    isa      => 'ArrayRef[WebService::Async::Request]',
    accessor => '_request_queue',
    default  => sub { [] },
    handles  => {
        count_request => 'count',
        pop_request   => 'pop',
        push_request  => 'push',
        all_requests  => 'elements',
    },
);

has all_parsed_responses => (
    is       => 'bare',
    isa      => 'WebService::Async::Result',
    accessor => '_all_parsed_responses',
    builder  => '_build_all_parsed_responses',
    handles  => {
        '_set_all_parsed_response'    => 'set',
        '_get_all_parsed_response'    => 'get',
        '_clear_all_parsed_responses' => 'clear',
    },
);

has response_parser => (
    is      => 'rw',
    does    => 'WebService::Async::Role::Parser',
    builder => '_build_response_parser',
    handles => { parse_response => 'parse', },
);

has response_converter => (
    is      => 'rw',
    does    => 'WebService::Async::Role::Converter',
    builder => '_build_response_converter',
    handles => { convert_response => 'convert', },
);

has whole_response_converter => (
    is      => 'rw',
    does    => 'WebService::Async::Role::Converter',
    builder => '_build_response_converter',
    handles => { convert_whole_response => 'convert', },
);

has logger => (
    is  => 'rw',
    isa => 'Log::Dispatch::Config',
);

has on_done => (
    is      => 'rw',
    isa     => 'CodeRef',
    default => sub {
        sub { }
    },
);

has known_agents => (
    traits  => ['Hash'],
    is      => 'bare',
    isa     => 'HashRef',
    handles => { get_known_agent => 'get', },
    default => sub {
        +{
            'Windows IE 6' =>
              'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)',
            'Windows Mozilla' =>
'Mozilla/5.0 (Windows; U; Windows NT 5.0; en-US; rv:1.4b) Gecko/20030516 Mozilla Firebird/0.6',
            'Mac Safari' =>
'Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en-us) AppleWebKit/85 (KHTML, like Gecko) Safari/85',
            'Mac Mozilla' =>
'Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.4a) Gecko/20030401',
            'Linux Mozilla' =>
              'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.4) Gecko/20030624',
            'Linux Konqueror' => 'Mozilla/5.0 (compatible; Konqueror/3; Linux)',
        };
    },
);

has user_agent => (
    is      => 'rw',
    isa     => 'Maybe[Str]',
    builder => '_build_user_agent',
);

has on_complete => (
    is      => 'rw',
    isa     => 'CodeRef',
    default => sub {
        sub { }
    },
);

has on_error => (
    is      => 'rw',
    isa     => 'CodeRef',
    default => sub {
        sub { }
    },
);

has on_critical_error => (
    is      => 'rw',
    isa     => 'CodeRef',
    default => sub {
        sub { }
    },
);

has check_error => (
    is      => 'rw',
    isa     => 'CodeRef',
    default => sub {
        sub { }
    },
);

has critical_error_message => (
    is  => 'rw',
    isa => 'Str',
);

before auto_block => sub {
    my ( $self, @args ) = @_;
    if ( $self->_is_busy && @args > 0 ) {
        confess
q{Cannot change the 'auto_block' attribute while the request queue is processing.};
    }
};

around user_agent => sub {
    my ( $next, $self, @args ) = @_;
    return $self->$next unless @args;
    my $alias = $args[0];
    my $agent = $self->get_known_agent($alias);
    $agent = $agent ? $agent : $alias;
    return $self->$next($agent);
};

no Moose;
no Moose::Util::TypeConstraints;

use Class::MOP;
use Smart::Args;
use Carp;
use Encode qw(decode_utf8);
use Scalar::Util 'weaken';
use Hash::MultiKey;
use AnyEvent;
use AnyEvent::HTTP;
use Log::Dispatch::Config;
use WebService::Async::Result;
use WebService::Async::Request;
use WebService::Async::ResponseCache;

sub get {
    my ( $self, @args ) = @_;
    $self->info(q{Invoke 'get' method.});
    return $self->_do_request( 'GET', @args );
}

sub post {
    my ( $self, @args ) = @_;
    $self->info(q{Invoke 'post' method.});
    return $self->_do_request( 'POST', @args );
}

sub add_get {
    my ( $self, @args ) = @_;
    $self->info(q{Invoke 'add_get' method.});
    $self->_add_request( 'GET', @args );
}

sub add_post {
    my ( $self, @args ) = @_;
    $self->info(q{Invoke 'add_post' method.});
    $self->_add_request( 'POST', @args );
}

sub send_request {
    args my $self, my $block => { isa => 'Bool', optional => 1 };

    $self->info(q{Invoke 'send_request' method.});

    if ( defined $block && $block ) {
        $self->auto_block(1);
    }
    $self->_process_request_queue;
    if ( $self->auto_block ) {
        return $self->_request_condvar->recv;
    }
}

sub BUILD {
    my ( $self, @init_args ) = @_;

    # create all log methods.
    __PACKAGE__->meta->make_mutable;
    for my $sub_name (
        qw(debug info notice warning error critical alert emergency))
    {
        __PACKAGE__->meta->add_method(
            $sub_name,
            sub {
                my ( $_self, $message ) = @_;
                return if !defined $_self->logger;
                $_self->logger->log( level => $sub_name, message => $message );
            }
        );
    }
    __PACKAGE__->meta->make_immutable;
}

sub _build_all_parsed_responses {
    args my $self;
    $self->_all_parsed_responses( WebService::Async::Result->new );
}

sub _build_response_parser {
    args my $self;
    require WebService::Async::Parser::Raw;
    $self->response_parser( WebService::Async::Parser::Raw->new );
}

sub _build_response_converter {
    args my $self;
    require WebService::Async::Converter::Raw;
    $self->response_converter( WebService::Async::Converter::Raw->new );
}

sub _build_whole_response_converter {
    args my $self;
    require WebService::Async::Converter::Raw;
    $self->whole_response_converter( WebService::Async::Converter::Raw->new );
}

sub _build_user_agent {
    args my $self;
    $self->user_agent('Windows IE 6');
}

sub _convert {
    args my $self, my $request => 'WebService::Async::Request',
      my $parsed_response => 'Object|HashRef|ArrayRef|Str',
      my $is_whole        => { isa => 'Bool', optional => 1 };
    if ( defined $is_whole && $is_whole ) {
        return $self->whole_response_converter->convert(
            async           => $self,
            request         => $request,
            parsed_response => $parsed_response
        );
    }
    return $self->response_converter->convert(
        async           => $self,
        request         => $request,
        parsed_response => $parsed_response
    );
}

sub _clear {
    args my $self;
    $self->_is_busy(0);
    $self->_clear_all_parsed_responses;
}

sub _build_request_condvar {
    args my $self;
    $self->_request_condvar(AE::cv);
}

sub _build_data_uuid {
    args my $self;
    require Data::UUID;
    $self->_data_uuid( Data::UUID->new );
}

sub _do_request {
    my $self   = shift;
    my $method = shift;

    if ( $self->_is_busy ) {
        $self->error(
'Cannot send the another request while the request queue is processing.'
        );
        confess
'Cannot send the another request while the request queue is processing.';
    }

    # search callbacks
    my @cb = grep { ref $_ eq 'CODE' } @_;
    my $on_complete = shift @cb;

    # extract other parameters
    my @args = grep { ref $_ ne 'CODE' } @_;
    my %args = @args;

    if ( defined $on_complete ) {
        $self->on_complete($on_complete);
    }

    return $self->_request(
        keys   => [ $self->_data_uuid->to_string( $self->_data_uuid->create ) ],
        method => $method,
        on_complete => $self->on_complete,
        param       => \%args
    );
}

sub _add_request {
    my $self   = shift;
    my $method = shift;

    if ( $self->_is_busy ) {
        $self->error(
'Cannot send the another request while the request queue is processing.'
        );
        confess
'Cannot send the another request while the request queue is processing.';
    }

    # search callbacks
    my @cb = grep { ref $_ eq 'CODE' } @_;
    my $on_done = shift @cb;

    # extract other parameters
    my @args = grep { ref $_ ne 'CODE' } @_;
    my %args = @args;

    # param => {}
    my @keys;
    my $param;
    if ( exists $args{param} && ref $args{param} eq 'HASH' ) {
        $param = delete $args{param};
        @keys = map { ( $_, $args{$_} ) } sort keys %args;
    }
    else {
        $param = \%args;
    }
    if ( !@keys ) {
        @keys = ( $self->_data_uuid->to_string( $self->_data_uuid->create ) );
    }

  ALL_EXISTS_LOOP:
    for my $req ( $self->all_requests ) {
        next ALL_EXISTS_LOOP if @keys != $req->count_keys;
        for ( my $i = 0 ; $i < @keys ; $i++ ) {
            next ALL_EXISTS_LOOP if $keys[$i] ne $req->get_key($i);
        }
        $self->error('The requst key is duplicate.');
        confess 'The requst key is duplicate.';
    }

    # add request
    my $request = $self->_create_request(
        keys    => \@keys,
        method  => $method,
        param   => $param,
        on_done => $on_done
    );
    $self->debug('Push a request into the request queue.');
    $self->push_request($request);
    return \@keys;
}

sub _create_request {
    args my $self, my $method => 'Str',
      my $keys    => { isa => 'ArrayRef',       optional => 1 },
      my $param   => { isa => 'HashRef',        optional => 1 },
      my $on_done => { isa => 'Maybe[CodeRef]', optional => 1 };

    my $request = WebService::Async::Request->new(
        keys    => $keys,
        method  => $method,
        url     => $self->base_url,
        on_done => $on_done,
    );
    $request->finalize( base_param => $self->param, option_param => $param );
    return $request;
}

sub _request {
    args my $self, my $method => 'Str',
      my $keys        => 'ArrayRef',
      my $on_complete => 'CodeRef',
      my $param       => { isa => 'HashRef', optional => 1 };
    my @args = ( keys => $keys, method => $method );
    if ( defined $param ) {
        push @args, ( param => $param );
    }
    my $request = $self->_create_request(@args);
    $self->on_complete($on_complete);
    $self->debug('Push a request into the request queue.');
    $self->push_request($request);
    $self->_process_request_queue;
    if ( $self->auto_block ) {
        return $self->_request_condvar->recv;
    }
}

sub _process_request_queue {
    my $self = shift;

    $self->debug('Start processing request queue.');

    # replace cv
    $self->_request_condvar(AE::cv);

    # set some counters
    my $request_count = my $all_request_count = $self->count_request;
    my $critical_error_counter = 0;

    # check whether connecting or not
    if ( $self->_is_busy ) {
        $self->error(
'Cannot send the another request while the request queue is processing.'
        );
        confess
'Cannot send the another request while the request queue is processing.';
    }
    if ($request_count) {
        $self->debug('Set the busy flag is true.');
        $self->_is_busy(1);
    }

    # a checking routine for the each response.
    my $done = sub {
        my ( $self, $success, $kill_guards, $id, $response, $request,
            $response_header_status, $is_critical )
          = @_;

        if ($is_critical) {
            $critical_error_counter++;
        }

        # killing guards
        for my $guard ( @{$kill_guards} ) {
            undef $guard;
        }

        # fire the on_done or on_error event
        if ($success) {
            $self->info(
q{One request is successfully completed. Execute 'on_done' callback.}
            );
            if ( defined $request && defined $request->on_done ) {
                $request->on_done->( $self, $id, $response, $request );
            }
            else {
                $self->on_done->( $self, $id, $response, $request );
            }
        }
        else {
            $self->warning(q{Request failed. Excecute 'on_error' callback.});
            $self->on_error->( $self, $response_header_status, $is_critical );
        }

        # fire the on_critical_error or on_complete event
        $request_count--;
        if ( $request_count <= 0 ) {

            # all requests failed with critical error
            if ( $all_request_count == $critical_error_counter ) {

                # clear the busy state and stored responses.
                $self->debug(
'The request is canceled with a critical error. Clear the busy flag.'
                );
                $self->debug(
'The request is canceled with a critical error. Clear all parsed responses.'
                );
                $self->_clear;

                # raise the exception after execution of $done and on_error.
                $self->error(
"HTTP connection error occured. The status code is '${response_header_status}'."
                );
                $self->on_critical_error->(
                    $self, $self->critical_error_message
                );
                $self->_request_condvar()
                  ->croak(
"HTTP connection error occured. The status code is '${response_header_status}'."
                  );
                return;    # skip later process
            }

            # all request successed
            my $all_converted_responses = $self->_convert(
                request         => $request,
                parsed_response => $self->_all_parsed_responses,
                is_whole        => 1,
            );

            # clear the busy state and stored responses.
            $self->debug(
                'The request is successfully completed. Clear the busy flag.');
            $self->debug(
'The request is successfully completed. Clear all parsed responses.'
            );
            $self->_clear;

            # fire the complete event and sending a signal.
            $self->info(
q{All the request is successfully completed. Execute 'on_complete' callback.}
            );
            $self->on_complete->( $self, $all_converted_responses );
            if ( $self->auto_block ) {
                $self->_request_condvar->send($all_converted_responses);
            }
        }
    };

    # process all requests
    while ( my $request = $self->pop_request ) {
        $self->debug( 'Start processing request: ' . $request->key_for_cache );
        my $keys          = $request->keys;
        my $url           = $request->url;
        my $method        = $request->method;
        my $retry_counter = $self->max_retry_count;
        my $timer;

        # checking, parsing, converting a cache and firing some events
        my $cache;
        if ( $self->response_cache ) {
            $cache = $self->response_cache->get_response(
                key => $request->key_for_cache );
        }
        if ( defined $cache ) {
            $self->info( 'Cache hit: ' . $request->key_for_cache );
            $self->debug('Parse a response.');
            my $parsed_response =
              $self->parse_response( response_body => $cache );
            $self->debug(
                'Set a parsed response into the WebService::Async::Result');
            $self->_set_all_parsed_response(
                key   => $keys,
                value => $parsed_response
            );
            $self->debug('Convert a parsed response to the specified format.');
            my $converted_response = $self->_convert(
                request         => $request,
                parsed_response => $parsed_response,
            );
            $done->( $self, SUCCESS, [], $keys, $converted_response, $request );

            next;    # skip later process
        }

        # does not hit any caches.
        $self->info( 'Does not hit any caches: ' . $request->key_for_cache );

# connection routine (caching, parsing, converting a response and firing some events)
        my $send_request;
        $send_request = sub {
            my $guard;

            # connection handler
            my $on_body = sub {
                my ( $body, $header ) = @_;
                $self->debug(
                    'Receive a response from ' . $request->key_for_cache );
                $body = decode_utf8($body);

                # retry on error
                my $is_critical_error = $header->{Status} !~ m{^2}xms;
                my $is_error          = !defined $body
                  || $self->check_error->( $header, $body );
                if ( $is_error || $is_critical_error ) {
                    $self->warning(
                        "An error occured. Status code=$header->{Status}");
                    undef $timer;
                    if ( $retry_counter-- <= 0 ) {

                        # firing events.
                        $done->(
                            $self, FAILURE,
                            [ $guard, $timer ], $keys,
                            undef,               $request,
                            $header->{'Status'}, $is_critical_error
                        );
                        return;
                    }
                    weaken $timer;
                    $timer ||= AE::timer $self->retry_interval, 0,
                      $send_request;
                    return;    # skip later process
                }

                # caching, parsing, converting a resopnse and firing events.
                if ( $self->response_cache ) {
                    $self->debug( 'Cache set at: ' . $request->key_for_cache );
                    $self->response_cache->set_response(
                        key   => $request->key_for_cache,
                        value => $body
                    );
                }
                $self->debug('Parse a response.');
                my $parsed_response =
                  $self->parse_response( response_body => $body );
                $self->debug('Set a parsed response into the internal store.');
                $self->_set_all_parsed_response(
                    key   => $keys,
                    value => $parsed_response
                );
                $self->debug(
                    'Convert a parsed response to the specified format.');
                my $converted_response = $self->_convert(
                    request         => $request,
                    parsed_response => $parsed_response,
                );

                # firing events
                $done->(
                    $self, SUCCESS, [ $guard, $timer ],
                    $keys, $converted_response, $request, $header->{'Status'}
                );
            };    # end of $on_body

            # send a request
            my $retry = $self->max_retry_count - $retry_counter;
            if ( $retry == 0 ) {
                $retry = '[first time]';
            }
            else {
                $retry = "[retry=${retry}]";
            }
            $self->debug(
"Invoking http_request method ${retry} (method=${method} url=${url})."
            );

            weaken $guard;
            $guard ||= http_request $method, $url,
              headers => {
                Accept       => '*/*',
                'User-Agent' => $self->user_agent,
                (
                    $method eq 'POST'
                    ? ( 'Content-Type' => 'application/x-www-form-urlencoded' )
                    : ()
                ),
              },
              timeout   => $self->timeout,
              body      => $request->body,
              on_header => sub {
                my ($head) = @_;
                return 0 if ref $head ne 'HASH';
                return 1;
              },
              on_body => $on_body;
        };    # end of $send_request

        weaken $timer;
        $timer ||= AE::timer 0, 0, $send_request;
    }
}
1;

__END__

#line 1579
