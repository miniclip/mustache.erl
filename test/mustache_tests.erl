%% The MIT License
%%
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% See the README at http://github.com/mojombo/mustache.erl for additional
%% documentation and usage examples.

-module(mustache_tests).

-include_lib("eunit/include/eunit.hrl").

-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-dialyzer({no_opaque, simple_test/0}). % because we're using a dict as input
simple_test() ->
    Ctx = dict:from_list([{name, "world"}]),
    Result = mustache:render("Hello {{name}}!", Ctx),
    ?assertEqual("Hello world!", Result).

-dialyzer({no_opaque, integer_values_too_test/0}). % because we're using a dict as input
integer_values_too_test() ->
    Ctx = dict:from_list([{name, "Chris"}, {value, 10000}]),
    Result = mustache:render("Hello {{name}}~nYou have just won ${{value}}!", Ctx),
    ?assertEqual("Hello Chris~nYou have just won $10000!", Result).

-dialyzer({no_opaque, specials_test/0}). % because we're using a dict as input
specials_test() ->
    Ctx = dict:from_list([{name, "Chris"}, {value, 10000}]),
    Result = mustache:render("\'Hello\n\"{{name}}\"~nYou \"have\" ju\0st\\ won\b\r\""
                             "${{value}}!\"\t", Ctx),
    ?assertEqual("\'Hello\n\"Chris\"~nYou \"have\" ju\0st\\ won\b\r\"$10000!\"\t", Result).

%% ===================================================================
%% basic tag types
%% ===================================================================

tag_type_variable_empty_test() ->
    test_helper("{{name}}", "", []).

tag_type_variable_string_test() ->
    test_helper("{{name}}", "NAME", [{name, "NAME"}]).

tag_type_variable_integer_test() ->
    test_helper("{{name}}", "1", [{name, 1}]).

tag_type_variable_atom_test() ->
    test_helper("{{name}}", "atom", [{name, atom}]).

tag_type_varibale_escaped_test() ->
    test_helper("{{name}}", "&gt;&amp;do&lt;it&gt;", [{name, ">&do<it>"}]).

tag_type_variabel_unescaped_test() ->
    test_helper("{{{name}}}", ">dont&do<it>", [{name, ">dont&do<it>"}]).

tag_type_variable_unescaped_with_ampersand_test() ->
    test_helper("{{&name}}", ">dont&do<it>", [{name, ">dont&do<it>"}]).


tag_type_section_empty_test() ->
    test_helper("{{#name}}section{{/name}}", "", []).

tag_type_section_false_test() ->
    test_helper("{{#name}}section{{/name}}", "", [{name, false}]).

tag_type_section_true_test() ->
    test_helper("{{#name}}section{{/name}}", "section", [{name, true}]).

tag_type_section_empty_list_test() ->
    test_helper("{{#name}}section{{/name}}", "", [{name, []}]).

tag_type_section_nonempty_list_test() ->
    CtxList = [{name, [ dict:new() || _ <- lists:seq(1, 3) ]}],
    test_helper("{{#name}}section{{/name}}", "sectionsectionsection", CtxList).


tag_type_inverted_section_empty_test() ->
    test_helper("{{^name}}section{{/name}}", "section", []).

tag_type_inverted_section_false_test() ->
    test_helper("{{^name}}section{{/name}}", "section", [{name, false}]).

tag_type_inverted_section_true_test() ->
    test_helper("{{^name}}section{{/name}}", "", [{name, true}]).

tag_type_inverted_section_empty_list_test() ->
    test_helper("{{^name}}section{{/name}}", "section", [{name, []}]).

tag_type_inverted_section_nonempty_list_test() ->
    CtxList = [{name, [ dict:new() || _ <- lists:seq(1, 3) ]}],
    test_helper("{{^name}}section{{/name}}", "", CtxList).


tag_type_comment_test() ->
    test_helper("{{!comment}}", "", []).

tag_type_comment_empty_test() ->
    test_helper("{{! }}", "", []).

tag_type_comment_multiline_test() ->
    test_helper("{{!\ncomment\ncomment\ncomment\n\n}}", "", []).


-dialyzer({no_opaque, test_helper/3}). % because we're using a dict as input
test_helper(Template, Expected, CtxList) ->
    Ctx = dict:from_list(CtxList),
    ?assertEqual(Expected, mustache:render(Template, Ctx)).

