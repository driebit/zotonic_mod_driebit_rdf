%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for Turtle serialization.
%% See also: https://www.w3.org/TR/rdf12-turtle/
%% @end

-module(rdf_turtle).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    serialize/2
]).

-spec serialize(rdf_graph(), z:context()) -> {ok, binary()} | {error, term()}.
serialize(RdfGraph, _Context) ->
    Lines = sets:fold(
        fun(Triple, Result) -> Result ++ serialize_triple(Triple) end,
        [],
        RdfGraph
    ),
    % Sort and remove duplicate/empty lines before joining them all together:
    {ok, lists:foldl(
        fun
            (<<>>, Acc) -> Acc;
            (Line, <<>>) -> Line;
            (Line, Acc) -> <<Acc/binary, "\n", Line/binary>>
        end,
        <<>>,
        lists:usort(Lines)
    )}.

-spec serialize_triple(#rdf_triple{}) -> list(binary()).
serialize_triple(#rdf_triple{subject = Subject, predicate = Predicate, object = Object} = RdfTriple) ->
    case Object of
        IRI when is_binary(IRI) ->
            to_line(
                serialize_subject(Subject),
                serialize_predicate(Predicate),
                serialize_iri(IRI)
            );
        blank ->
            [];
        #rdf_literal{} = Literal ->
            to_line(
                serialize_subject(Subject),
                serialize_predicate(Predicate),
                serialize_literal(Literal)
            );
        #rdf_triple{subject = blank} = TripleTerm ->
            BlankNode = blank_node({Subject, Predicate}),
            Referencing = serialize_triple(RdfTriple#rdf_triple{object = BlankNode}),
            ObjectTriple = serialize_triple(TripleTerm#rdf_triple{subject = BlankNode}),
            Referencing ++ ObjectTriple;

        #rdf_triple{subject = IRI} = TripleTerm when is_binary(IRI) ->
            Referencing = serialize_triple(RdfTriple#rdf_triple{object = IRI}),
            ObjectTriple = serialize_triple(TripleTerm),
            Referencing ++ ObjectTriple
    end.

-spec to_line(binary(), binary(), binary()) -> list(binary()).
to_line(Subject, Predicate, Object) ->
    [<<Subject/binary, " ", Predicate/binary, " ", Object/binary, ".">>].

-spec serialize_subject(iri() | blank) -> binary().
serialize_subject(blank) ->
    <<"[]">>;
serialize_subject(IRI) when is_binary(IRI) ->
    serialize_iri(IRI).

-spec serialize_predicate(iri()) -> binary().
serialize_predicate(Predicate) ->
    serialize_iri(Predicate).

-spec serialize_iri(iri()) -> binary().
serialize_iri(IRI = <<"http://", _/binary>>) ->
    <<"<", IRI/binary, ">">>;
serialize_iri(IRI = <<"https://", _/binary>>) ->
    <<"<", IRI/binary, ">">>;
serialize_iri(IRI) when is_binary(IRI) ->
    %% Prefixed IRI or blank node identifier:
    IRI.

-spec serialize_literal(#rdf_literal{}) -> binary().
% https://www.w3.org/TR/rdf12-turtle/#literals
% treat all literals with a language tag as strings and all string datatype IRIs the same way:
serialize_literal(#rdf_literal{datatype_iri = DatatypeIri, language_tag = LanguageTag} = Literal)
when DatatypeIri =:= <<"http://www.w3.org/2001/XMLSchema#string">>
orelse DatatypeIri =:= <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>
orelse DatatypeIri =:= <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#dirLangString">>
orelse LanguageTag =/= undefined
->
    ValueString = serialize_lexical_form(Literal#rdf_literal.lexical_form),
    LanguageString = case LanguageTag of
        undefined ->
            <<"">>;
        _ ->
            TagString = <<"@", LanguageTag/binary>>,
            case Literal#rdf_literal.base_direction of
                undefined ->
                    TagString;
                Direction when is_atom(Direction) ->
                    DirectionString = z_convert:to_binary(Direction),
                    <<TagString/binary, "--", DirectionString/binary>>
            end
        end,
    <<ValueString/binary, LanguageString/binary>>;
% https://www.w3.org/TR/rdf12-turtle/#abbrev
% treat all numeric datatype IRIs with a shorthand the same way:
serialize_literal(#rdf_literal{datatype_iri = DatatypeIri} = Literal)
when DatatypeIri =:= <<"http://www.w3.org/2001/XMLSchema#integer">>
orelse DatatypeIri =:= <<"http://www.w3.org/2001/XMLSchema#decimal">>
orelse DatatypeIri =:= <<"http://www.w3.org/2001/XMLSchema#double">>
->
    Literal#rdf_literal.lexical_form;
% https://www.w3.org/TR/rdf12-turtle/#turtle-literals
% otherwise, treat them as quoted literals with a datatype IRI:
serialize_literal(#rdf_literal{lexical_form = LF, datatype_iri = DatatypeIri}) ->
    ValueString = serialize_lexical_form(LF),
    DatatypeString = serialize_iri(DatatypeIri),
    <<ValueString/binary, "^^", DatatypeString/binary>>.

-spec serialize_lexical_form(binary()) -> binary().
serialize_lexical_form(LexicalForm) when is_binary(LexicalForm) ->
    Escaped = escape(LexicalForm),
    <<$", Escaped/binary, $">>.

% Produce a blank node identifier: https://www.w3.org/TR/rdf12-turtle/#BNodes
% Note: this is constant, it always returns the same string for a given input.
-spec blank_node(term()) -> binary().
blank_node(Term) ->
    <<"_:", (erlang:integer_to_binary(erlang:phash2(Term)))/binary>>.

-spec escape(binary()) -> binary().
escape(<<>>) ->
    <<>>;
escape(<<$\b, Rest/binary>>) ->
    <<$\\, $b, (escape(Rest))/binary>>;
escape(<<$\t, Rest/binary>>) ->
    <<$\\, $t, (escape(Rest))/binary>>;
escape(<<$\n, Rest/binary>>) ->
    <<$\\, $n, (escape(Rest))/binary>>;
escape(<<$\f, Rest/binary>>) ->
    <<$\\, $f, (escape(Rest))/binary>>;
escape(<<$\r, Rest/binary>>) ->
    <<$\\, $r, (escape(Rest))/binary>>;
escape(<<$", Rest/binary>>) ->
    <<$\\, $", (escape(Rest))/binary>>;
escape(<<$', Rest/binary>>) ->
    <<$\\, $', (escape(Rest))/binary>>;
escape(<<$\\, Rest/binary>>) ->
    <<$\\, $\\, (escape(Rest))/binary>>;
escape(<<Char/utf8, Rest/binary>>) ->
    <<Char/utf8, (escape(Rest))/binary>>.
