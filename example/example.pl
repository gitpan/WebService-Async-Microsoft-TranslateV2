#!perl
use warnings;
use strict;
use AnyEvent;
use Data::Section::Simple qw(get_data_section);
use Try::Tiny;
use WebService::Async::Microsoft::TranslateV2;

# setup
my $s = WebService::Async::Microsoft::TranslateV2->new(
    app_id => '[YOUR BING APPLICATION ID]',
);
$s->single_template(get_data_section('single'));
$s->whole_template(get_data_section('whole'));
$s->critical_error_template(get_data_section('critical_error'));

# translate
$s->source_language('en');
$s->set_destination_languages(qw(it fr));
$s->set_message( message1 => 'apple' );
$s->set_message( message2 => 'banana' );
$s->set_message( message3 => 'orange' );
try {
    $s->translate(
        on_each_translation => sub {
            my ($self, $id, $res) = @_;
            # The single formatted message here.
            print $res;
        },
        on_translation_complete => sub {
            my ($self, $all_res) = @_;
            # The whole formatted message here.
            print $all_res;
        },
        on_critical_error => sub {
            my ($self, $message) = @_;
            # The critical error message here.
            print $message;
        }
    ); # blocking here.
}
catch {
    print $_;
};

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
