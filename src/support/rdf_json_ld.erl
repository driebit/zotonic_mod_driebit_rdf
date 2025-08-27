%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for JSON-LD serialization.
%% See also: https://www.w3.org/TR/json-ld11/
%% @end


-module(rdf_json_ld).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    serialize/2,
    serialize/3
]).

-spec serialize(rdf_graph(), z:context()) -> {ok, binary()} | {error, term()}.
serialize(RdfGraph, Context) ->
    serialize(RdfGraph, #{}, Context).

-spec serialize(rdf_graph(), map(), z:context()) -> {ok, binary()} | {error, term()}.
serialize(RdfGraph, NamespaceMap, _Context) ->
    {ok, jsxrecord:encode(graph_to_jsonld(RdfGraph, NamespaceMap))}.

%% @doc Serialize an RDF graph into a nested map for subsequent serialization to JSON.
%% In expanded form (https://w3c.github.io/json-ld-syntax/#expanded-document-form).
-spec graph_to_jsonld(rdf_graph(), map()) -> map().
graph_to_jsonld(RdfGraph, NamespaceMap) ->
    sets:fold(
        fun(Triple, Result) ->
            TripleResult = triple_to_jsonld(Triple, NamespaceMap),
            merge_values(TripleResult, Result)
        end,
        map_with_context(NamespaceMap),
        RdfGraph
    ).

-spec map_with_context(map()) -> map().
map_with_context(NamespaceMap) ->
    case maps:to_list(NamespaceMap) of
        [] ->
            #{};
        NamespaceList ->
            #{<<"@context">> =>
                maps:from_list(lists:map(
                    fun
                        ({undefined, NamespaceIRI}) ->
                            {<<"@vocab">>, NamespaceIRI};
                        ({NamespacePrefix, NamespaceIRI}) ->
                            {NamespacePrefix, NamespaceIRI}
                    end,
                    NamespaceList
                ))
            }
    end.


-spec triple_to_jsonld(#rdf_triple{}, map()) -> map().
triple_to_jsonld(#rdf_triple{subject = Subject, predicate = Predicate, object = Object}, NamespaceMap) ->
    SubjectMap = case Subject of
        SubjectIRI when is_binary(SubjectIRI) ->
            #{<<"@id">> => namespaced(SubjectIRI, NamespaceMap)};
        blank ->
            #{}
    end,
    case Object of
        ObjectIRI when is_binary(ObjectIRI) andalso Predicate =:= <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">> ->
            maps:put(<<"@type">>, namespaced(ObjectIRI, NamespaceMap), SubjectMap);
        ObjectIRI when is_binary(ObjectIRI) ->
            maps:put(namespaced(Predicate, NamespaceMap), [#{<<"@id">> => namespaced(ObjectIRI, NamespaceMap)}], SubjectMap);
        blank ->
            SubjectMap;
        #rdf_literal{} = Literal ->
            maps:put(namespaced(Predicate, NamespaceMap), [literal_to_jsonld(Literal, NamespaceMap)], SubjectMap);
        #rdf_triple{} ->
            maps:put(namespaced(Predicate, NamespaceMap), [triple_to_jsonld(Object, NamespaceMap)], SubjectMap)
    end.

%% value objects: https://w3c.github.io/json-ld-syntax/#value-objects
-spec literal_to_jsonld(#rdf_literal{}, map()) -> map().
literal_to_jsonld(#rdf_literal{language_tag = undefined} = Literal, NamespaceMap) ->
    #{
        <<"@value">> => Literal#rdf_literal.lexical_form,
        <<"@type">> => namespaced(Literal#rdf_literal.datatype_iri, NamespaceMap)
    };
literal_to_jsonld(#rdf_literal{base_direction = undefined} = Literal, _NamespaceMap) ->
    #{
        <<"@value">> => Literal#rdf_literal.lexical_form,
        <<"@language">> => Literal#rdf_literal.language_tag
    };
literal_to_jsonld(Literal, _NamespaceMap) ->
    #{
        <<"@value">> => Literal#rdf_literal.lexical_form,
        <<"@language">> => Literal#rdf_literal.language_tag,
        <<"@direction">> => z_convert:to_binary(Literal#rdf_literal.base_direction)
    }.

-spec namespaced(iri(), map()) -> iri().
namespaced(IRI, NamespaceMap) ->
    case rdf_utils:prefixed_iri(IRI, NamespaceMap) of
        undefined -> IRI;
        PrefixedIRI -> PrefixedIRI
    end.

%% @doc Merge a map into an accumulator map, combining multiple values for the
%% same key in lists.
-spec merge_values(map(), map()) -> map().
merge_values(Map, Acc) ->
    maps:merge_with(fun merge_values_with/3, Map, Acc).


merge_values_with(_Key, Val1, Val2) when Val1 =:= Val2 ->
    Val1;
merge_values_with(_Key, Val1, Val2) when is_list(Val1) andalso is_list(Val2) ->
    lists:foldl(fun merge_value_into_list/2, Val1, Val2);
merge_values_with(_Key, Val1, Val2) when is_list(Val1) ->
    merge_value_into_list(Val2, Val1);
merge_values_with(_Key, Val1, Val2) when is_list(Val2) ->
    merge_value_into_list(Val1, Val2);
merge_values_with(_Key, Val1, Val2) ->
    merge_value_into_list(Val1, [Val2]).

merge_value_into_list(Elem, []) ->
    [Elem];
merge_value_into_list(Elem, [Elem | Rest]) ->
    [Elem | Rest];
merge_value_into_list(#{<<"@id">> := SameId} = Elem, [#{<<"@id">> := SameId} = Head | Rest]) ->
    [merge_values(Elem, Head) | Rest];
merge_value_into_list(Elem, [Head | Rest]) ->
    [Head | merge_value_into_list(Elem, Rest)].

