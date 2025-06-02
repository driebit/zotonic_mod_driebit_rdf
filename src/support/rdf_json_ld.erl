%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(rdf_json_ld).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    serialize/2
]).

-spec serialize(rdf_graph(), z:context()) -> {ok, binary()} | {error, term()}.
serialize(RdfGraph, _Context) ->
    {ok, jsxrecord:encode(graph_to_jsonld(RdfGraph))}.

%% @doc Serialize an RDF graph into a nested map for subsequent serialization to JSON.
%% In expanded form (https://w3c.github.io/json-ld-syntax/#expanded-document-form).
-spec graph_to_jsonld(rdf_graph()) -> map().
graph_to_jsonld(RdfGraph) ->
    sets:fold(
        fun(Triple, Result) ->
            TripleResult = triple_to_jsonld(Triple),
            merge_values(TripleResult, Result)
        end,
        #{},
        RdfGraph
    ).

-spec triple_to_jsonld(#rdf_triple{}) -> map().
triple_to_jsonld(#rdf_triple{subject = Subject, predicate = Predicate, object = Object}) ->
    SubjectMap = case Subject of
        SubjectIRI when is_binary(SubjectIRI) ->
            #{<<"@id">> => SubjectIRI};
        blank ->
            #{}
    end,
    case Object of
        ObjectIRI when is_binary(ObjectIRI) andalso Predicate =:= <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">> ->
            maps:put(<<"@type">>, ObjectIRI, SubjectMap);
        ObjectIRI when is_binary(ObjectIRI) ->
            maps:put(Predicate, [#{<<"@id">> => ObjectIRI}], SubjectMap);
        blank ->
            SubjectMap;
        #rdf_literal{} = Literal ->
            maps:put(Predicate, [literal_to_jsonld(Literal)], SubjectMap);
        #rdf_triple{} ->
            maps:put(Predicate, [triple_to_jsonld(Object)], SubjectMap)
    end.

%% value objects: https://w3c.github.io/json-ld-syntax/#value-objects
-spec literal_to_jsonld(#rdf_literal{}) -> map().
literal_to_jsonld(#rdf_literal{language_tag = undefined} = Literal) ->
    #{
        <<"@value">> => Literal#rdf_literal.lexical_form,
        <<"@type">> => Literal#rdf_literal.datatype_iri
    };
literal_to_jsonld(#rdf_literal{base_direction = undefined} = Literal) ->
    #{
        <<"@value">> => Literal#rdf_literal.lexical_form,
        <<"@language">> => Literal#rdf_literal.language_tag
    };
literal_to_jsonld(Literal) ->
    #{
        <<"@value">> => Literal#rdf_literal.lexical_form,
        <<"@language">> => Literal#rdf_literal.language_tag,
        <<"@direction">> => z_convert:to_binary(Literal#rdf_literal.base_direction)
    }.

%% @doc Merge a map into an accumulator map, combining multiple list values for
%% the same key.
-spec merge_values(map(), map()) -> map().
merge_values(Map, Acc) ->
    maps:merge_with(
        fun
            (_Key, Val1, Val2) when is_list(Val1) andalso is_list(Val2) ->
                Val1 ++ Val2;
            (_Key, Val1, Val2) when Val1 =:= Val2 ->
                Val1
        end,
        Map,
        Acc
    ).
