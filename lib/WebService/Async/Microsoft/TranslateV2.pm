package WebService::Async::Microsoft::TranslateV2;
use Moose;
extends 'WebService::Async';

use Moose::Util::TypeConstraints;
use Smart::Args;
use Encode qw(decode_utf8);
use Try::Tiny;
use Data::Section::Simple qw(get_data_section);
use Text::MicroTemplate qw(:all);
use WebService::Async::Converter::Function;

use 5.008008;
our $VERSION = '0.02';

use constant TRANSLATION_API =>
  'http://api.microsofttranslator.com/V2/Ajax.svc/Translate';

### Attributes for the request
enum 'WebService::Async::Microsoft::TranslateV2::Languages' =>
  qw(aa ab af am ar as ay az ba be bg bh bi bn bo br ca co cs cy da de
  dz el en eo es et eu fa fi fj fo fr fy ga gd gl gn gu gv ha he hi hr
  hu hy ia id ie ik is it iu ja jv ka kk kl km kn ko ks ku ky la li ln
  lo lt lv mg mi mk ml mn mo mr ms mt my na ne nl no oc om or pa pl ps
  pt qu rm rn ro ru rw sa sd sg sh si sk sl sm sn so sq sr ss st su sv
  sw ta te tg th ti tk tl tn to tr ts tt tw ug uk ur uz vi vo wo xh yi
  yo zh zu);

has app_id => (
    is  => 'rw',
    isa => 'Str',
);

has messages => (
    traits  => ['Hash'],
    is      => 'rw',
    isa     => 'HashRef[Str]',
    default => sub { +{} },
    handles => {
        set_message      => 'set',
        get_message      => 'get',
        all_messages_ids => 'keys',
    },
);

has source_language => (
    is  => 'rw',
    isa => 'WebService::Async::Microsoft::TranslateV2::Languages',
);

has destination_languages => (
    traits  => ['Array'],
    is      => 'rw',
    isa     => 'ArrayRef[WebService::Async::Microsoft::TranslateV2::Languages]',
    default => sub { [] },
    handles => {
        all_destination_languages => 'elements',
        set_destination_languages => 'push',
    },
);

### Attributes for the response
has single_template => (
    is      => 'rw',
    isa     => 'Str',
    builder => '_build_single_template',
);

has whole_template => (
    is      => 'rw',
    isa     => 'Str',
    builder => '_build_whole_template',
);

has critical_error_template => (
    is      => 'rw',
    isa     => 'Str',
    builder => '_build_critical_error_template',
    trigger => sub { shift->critical_error_message(shift); },
);

sub BUILD {
    args my $self;
    $self->base_url(TRANSLATION_API);
    $self->param( {} );
    $self->response_converter(
        WebService::Async::Converter::Function->new(
            converter => \&_formatter
        )
    );
    $self->whole_response_converter(
        WebService::Async::Converter::Function->new(
            converter => \&_whole_formatter
        )
    );
}

sub _build_single_template {
    args my $self;
    $self->single_template(
        decode_utf8( get_data_section('single_template') ) );
}

sub _build_whole_template {
    args my $self;
    $self->whole_template( get_data_section('whole_template') );
}

sub _build_critical_error_template {
    args my $self;
    $self->critical_error_template( get_data_section('error_template') );
    $self->critical_error_message( $self->critical_error_template );
}

sub get_translated_message {
    args my $self, my $id => 'Str', my $language => 'Str';
    my $text = $self->_get_all_parsed_response( [ id => $id, language => $language ] );
    return $text;
}

sub set_trasnlated_message {
    args my $self, my $id => 'Str', my $language => 'Str', my $value => 'Str';
    $self->_all_parsed_responses->set(key => {id => $id, language => $language}, value => $value);
}

sub _cleanup {
    my $value = shift;
    $value =~ s{^[^"]+"(.*)[ ]"$}{$1}xms;
    return $value;
}

sub _formatter {
    my ( $self, $request, $parsed_response ) = @_;
    my $id         = $request->get_keys_as_hash->{id};
    my $lang       = $request->get_keys_as_hash->{language};
    my $translated = $parsed_response || '';
    $translated = _cleanup($translated);
    my $converted_reponse =
      render_mt( $self->single_template, $id, $lang, $translated )->as_string;
    return $converted_reponse;
}

sub _whole_formatter {
    my ( $self, $request, $parsed_response ) = @_;
    for my $key ($self->all_messages_ids) {
        for my $lang ($self->all_destination_languages) {
            my $text = $self->get_translated_message(id => $key, language => $lang);
            $text = _cleanup($text);
            $self->set_trasnlated_message(id => $key, language => $lang, value => $text);
        }
    }
    my $converted_reponse =
      render_mt( $self->whole_template, $self, $parsed_response )->as_string;
    return $converted_reponse;
}

sub translate {
    args my $self,
      my $on_each_translation     => { isa => 'CodeRef', optional => 1 },
      my $on_translation_complete => { isa => 'CodeRef', optional => 1 },
      my $on_critical_error       => { isa => 'CodeRef', optional => 1 };

    if ( defined $on_critical_error ) {
        $self->on_critical_error($on_critical_error);
    }
    if ( defined $on_each_translation ) {
        $self->on_done($on_each_translation);
    }
    if ( defined $on_translation_complete ) {
        $self->on_complete($on_translation_complete);
    }
    for my $id ( $self->all_messages_ids ) {
        for my $lang ( $self->all_destination_languages ) {
            $self->add_get(
                id       => $id,
                language => $lang,
                param    => {
                    appId => $self->app_id,
                    text  => $self->get_message($id),
                    from  => $self->source_language,
                    to    => $lang,
                }
            );
        }
    }

    try {
        $self->send_request;
    }
    catch {
        if ( $_ =~ m{^HTTP[ ]connection[ ]error}xms ) {
            print "CONNECTION ERROR!!";
        }
    };
}

__PACKAGE__->meta->make_immutable;
no Moose;

1;

__DATA__
@@ single_template
?= Text::MicroTemplate::encoded_string '<?xml version="1.0" encoding="UTF-8"?>'
? my ($id, $lang, $message) = @_
<result id="<?= $id ?>">
    <translated lang="<?= $lang ?>"><?= $message ?></translated>
</result>
?= "\0"

@@ whole_template
?= Text::MicroTemplate::encoded_string '<?xml version="1.0" encoding="UTF-8"?>'
? my ($self, $messages) = @_;
<results>
? for my $key ($self->all_messages_ids) {
    <result id="<?= $key ?>">
?   for my $lang ($self->all_destination_languages) {
?       my $text = $self->get_translated_message(id => $key, language => $lang);
        <translated lang="<?= $lang ?>"><?= $text || '' ?></translated>
?   }
    </result>
? }
</results>
?= "\0"

@@ error_template
?= Text::MicroTemplate::encoded_string '<?xml version="1.0" encoding="UTF-8"?>'
<results status="critical"/>
?= "\0"

__END__

=head1 NAME

WebService::Async::Microsoft::TranslateV2 - Subclass of WebService::Async that has a simple interface to Microsoft Translate Service.

=head1 SYNOPSIS

=head2 Simple usage

=head3 SOURCE

  use WebService::Async::Microsoft::TranslateV2;
  my $translator = WebService::Async::Microsoft::TranslateV2->new(
    app_id => '[YOUR BING APPLICATION ID]',
  );
  $translator->source_language('en');
  $translator->set_destination_languages(qw(it fr));
  $translator->set_message( message1 => 'apple' );
  $translator->set_message( message2 => 'banana' );
  $translator->set_message( message3 => 'orange' );
  $translator->translate(
      on_each_translation => sub {
          my ($self, $id, $res) = @_;
          print $res;
      },
      on_translation_complete => sub {
          my ($self, $all_res) = @_;
          print $all_res;
      },
  );

=head3 RESULTS

  <?xml version="1.0" encoding="UTF-8"?>
  <result id="message3">
      <translated lang="it">arancione</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result id="message1">
      <translated lang="fr">Apple</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result id="message2">
      <translated lang="fr">la banane</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result id="message2">
      <translated lang="it">banana</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result id="message3">
      <translated lang="fr">orange</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result id="message1">
      <translated lang="it">mela</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <results>
      <result id="message2">
          <translated lang="it">banana</translated>
          <translated lang="fr">la banane</translated>
      </result>
      <result id="message3">
          <translated lang="it">arancione</translated>
          <translated lang="fr">orange</translated>
      </result>
      <result id="message1">
          <translated lang="it">mela</translated>
          <translated lang="fr">Apple</translated>
      </result>
  </results>

=head2 Using custom templates

=head3 SOURCE

  use WebService::Async::Microsoft::TranslateV2;
  my $translator = WebService::Async::Microsoft::TranslateV2->new(
    app_id => '[YOUR BING APPLICATION ID]',
  );

  $translator->single_template(get_data_section('single'));
  $translator->whole_template(get_data_section('whole'));
  $translator->critical_error_template(get_data_section('critical_error'));

  $translator->source_language('en');
  $translator->set_destination_languages(qw(it fr));
  $translator->set_message( message1 => 'apple' );
  $translator->set_message( message2 => 'banana' );
  $translator->set_message( message3 => 'orange' );
  $translator->translate(
      on_each_translation => sub {
          my ($self, $id, $res) = @_;
          print $res;
      },
      on_translation_complete => sub {
          my ($self, $all_res) = @_;
          print $all_res;
      },
  );

  __DATA__  
  @@ single
  ?= Text::MicroTemplate::encoded_string '<?xml version="1.0" encoding="UTF-8"?>'
  ? my ($id, $lang, $message) = @_
  <result key="<?= $id ?>">
      <translated lang="<?= $lang ?>"><?= $message ?></translated>
  </result>
  ?= "\0"
  
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
  ?= "\0"

  @@ critical_error
  CRITICAL ERROR

=head3 RESULTS

  <?xml version="1.0" encoding="UTF-8"?>
  <result key="message3">
      <translated lang="it">arancione</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result key="message1">
      <translated lang="fr">Apple</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result key="message2">
      <translated lang="fr">la banane</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result key="message2">
      <translated lang="it">banana</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result key="message3">
      <translated lang="fr">orange</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <result key="message1">
      <translated lang="it">mela</translated>
  </result>
  
  <?xml version="1.0" encoding="UTF-8"?>
  <results>
      <result key="message2">
          <translated lang="it">banana</translated>
          <translated lang="fr">la banane</translated>
      </result>
      <result key="message3">
          <translated lang="it">arancione</translated>
          <translated lang="fr">orange</translated>
      </result>
      <result key="message1">
          <translated lang="it">mela</translated>
          <translated lang="fr">Apple</translated>
      </result>
  </results>

=head1 DESCRIPTION

WebService::Async::Microsoft::TranslateV2 is subclass of WebService::Async that has a simple interface to Microsoft Translate Service.

=head1 METHODS

=head2 source_language

Sets the source language.

  $translator->source_language('en');

=head2 set_destination_languages

Sets destination languages.

  $translator->set_destination_languages(qw(it fr));

=head2 set_message

Sets messages that you want to translate.

  $translator->set_message( message1 => 'apple' );

=head2 translate

  $translator->translate(
      on_each_translation => sub {
          my ($self, $id, $res) = @_;
      },
      on_translation_complete => sub {
          my ($self, $all_res) = @_;
      },
  );

=head1 AUTHOR

keroyon E<lt>keroyon@cpan.orgE<gt>

=head1 SEE ALSO

=over

=item L<WebService::Async>

=back

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
