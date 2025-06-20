%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Helpers for common namespaces used in the specification, see:
%% https://www.w3.org/TR/rdf12-concepts/#vocabularies
%% @end

-module(rdf_xsd).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    namespaced_iri/1,
    rdf_namespaced_iri/1,
    rdfs_namespaced_iri/1
]).

% Namespace the IRI for the given term to 'xsd'/'http://www.w3.org/2001/XMLSchema'.
-spec namespaced_iri(atom() | binary()) -> iri().
namespaced_iri(TermName) when is_atom(TermName) ->
    namespaced_iri(z_convert:to_binary(TermName));
namespaced_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/2001/XMLSchema#", TermName/binary>>.

% Namespace the IRI for the given term to 'rdf'/'http://www.w3.org/1999/02/22-rdf-syntax-ns'.
-spec rdf_namespaced_iri(atom() | binary()) -> iri().
rdf_namespaced_iri(TermName) when is_atom(TermName) ->
    rdf_namespaced_iri(z_convert:to_binary(TermName));
rdf_namespaced_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#", TermName/binary>>.

% Namespace the IRI for the given term to 'rdfs'/'http://www.w3.org/2000/01/rdf-schema'.
-spec rdfs_namespaced_iri(atom() | binary()) -> iri().
rdfs_namespaced_iri(TermName) when is_atom(TermName) ->
    rdfs_namespaced_iri(z_convert:to_binary(TermName));
rdfs_namespaced_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/2000/01/rdf-schema#", TermName/binary>>.
