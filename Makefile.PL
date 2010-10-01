use inc::Module::Install;
name 'WebService-Async-Microsoft-TranslateV2';
all_from 'lib/WebService/Async/Microsoft/TranslateV2.pm';
readme_from 'lib/WebService/Async/Microsoft/TranslateV2.pm';
readme_markdown_from 'lib/WebService/Async/Microsoft/TranslateV2.pm';
readme_pod_from 'lib/WebService/Async/Microsoft/TranslateV2.pm';
githubmeta;

tests 't/*.t';
author_tests 'xt';

build_requires 'Test::More';
build_requires 'IO::Prompt';
build_requires 'WebService::Async';
auto_set_repository;
auto_include;
WriteAll;
