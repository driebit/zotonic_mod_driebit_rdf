%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(rdf_xsd).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    namespace_iri/1,
    rdf_namespace_iri/1,
    rdfs_namespace_iri/1
]).

% https://www.w3.org/TR/rdf12-concepts/#vocabularies
-spec namespace_iri(atom() | binary()) -> iri().
namespace_iri(TermName) when is_atom(TermName) ->
    namespace_iri(z_convert:to_binary(TermName));
namespace_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/2001/XMLSchema#", TermName/binary>>.

-spec rdf_namespace_iri(atom() | binary()) -> iri().
rdf_namespace_iri(TermName) when is_atom(TermName) ->
    rdf_namespace_iri(z_convert:to_binary(TermName));
rdf_namespace_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#", TermName/binary>>.

-spec rdfs_namespace_iri(atom() | binary()) -> iri().
rdfs_namespace_iri(TermName) when is_atom(TermName) ->
    rdfs_namespace_iri(z_convert:to_binary(TermName));
rdfs_namespace_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/2000/01/rdf-schema#", TermName/binary>>.
