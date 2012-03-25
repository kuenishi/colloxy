
% This file is auto-generated from /home/kuenishi/src/jubatus.github/src/server/recommender.idl
% *** DO NOT EDIT ***


-ifndef(RECOMMENDER).
-define(RECOMMENDER, 1).


-type param_t() :: list({string(), string()}).

-type similar_result() :: list({string(), float()}).

-type string_rule() :: [
      string() %  key
    | string() %  t
    | string() %  sample_weight
    | string() %  global_weight
    ]. % 

-type filter_rule() :: [
      string() %  key
    | string() %  t
    | string() %  suffix
    ]. % 

-type num_rule() :: [
      string() %  key
    | string() %  t
    ]. % 

-type converter_config() :: [
      list({string(), param_t()}) %  string_filter_types
    | list(filter_rule()) %  string_filter_rules
    | list({string(), param_t()}) %  num_filter_types
    | list(filter_rule()) %  num_filter_rules
    | list({string(), param_t()}) %  string_types
    | list(string_rule()) %  string_rules
    | list({string(), param_t()}) %  num_types
    | list(num_rule()) %  num_rules
    ]. % 

-type config_data() :: [
      string() %  method
    | converter_config() %  converter
    ]. % 

-type datum() :: [
      list({string(), string()}) %  sv
    | list({string(), float()}) %  nv
    ]. % 


-endif.


