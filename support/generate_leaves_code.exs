#!/usr/bin/env elixir

defmodule Generator do
  require Logger

  ## Types

  ## API

  def run(btree_order, mode) do
    depth_flattening = 2
    max_root_keys = min_child_leaf_keys(btree_order, depth_flattening) - 1
    max_keys = max_keys(btree_order, depth_flattening)

    get_root_definitions = get_root_definitions(max_root_keys)
    get_definitions = get_definitions(max_root_keys, max_keys)

    insert_root_definitions = insert_root_definitions(max_root_keys, max_keys)
    insert_definitions = insert_definitions(max_root_keys, max_keys)

    template = 
    """
    -module(b5_leaves).

    %% ------------------------------------------------------------------
    %% API Function Exports
    %% ------------------------------------------------------------------

    -export([
      get_root/2,
      get/2,
      insert_root/4,
      insert/4
    ]).

    %% ------------------------------------------------------------------
    %% Macro Definitions
    %% ------------------------------------------------------------------

    #{macro_definitions(max_keys)}

    %% ------------------------------------------------------------------
    %% API Function Definitions
    %% ------------------------------------------------------------------

    #{get_root_definitions}

    #{get_definitions}

    #{insert_root_definitions}

    #{insert_definitions}

    %% ------------------------------------------------------------------
    %% Internal Function Definitions
    %% ------------------------------------------------------------------

    eval_insert_value(eager, Value) -> Value;
    eval_insert_value(lazy, Fun) -> Fun().
    """

    case mode do
      :print ->
        IO.puts(template)

      :write ->
        File.write!("src/b5_leaves.erl", template)
        System.cmd("make", ["format"])
    end
  end

  ## Internal

  defp min_child_leaf_keys(btree_order, depth_flattening) when depth_flattening > 1 do
    nr_of_keys = div(btree_order, 2)
    nr_of_children = nr_of_keys + 1
    nr_of_keys + (nr_of_children * min_child_leaf_keys(btree_order, depth_flattening - 1))
  end

  defp min_child_leaf_keys(btree_order, 1) do
    div(btree_order, 2)
  end

  defp max_keys(btree_order, depth_flattening) when depth_flattening > 1 do
    (btree_order - 1) + (btree_order * max_keys(btree_order, depth_flattening - 1))
  end

  defp max_keys(btree_order, 1) do
    btree_order - 1
  end

  defp macro_definitions(max_keys) do
    0..max_keys//1
    |> Enum.map(&macro_definitions_for_size/1)
    |> Enum.join("\n")
  end

  defp macro_definitions_for_size(nr_of_keys) do
    <<"?", build_name::bytes>> = macro_build_name(nr_of_keys)
    <<"?", match_name::bytes>> = macro_match_name(nr_of_keys)

    wrap_type = macro_wrap_type(nr_of_keys)
    kv_args = kv_args(nr_of_keys)
    kv_args_str = Enum.join(kv_args, ", ")
    wrapped_str = macro_wrap(wrap_type, kv_args)

    maybe_macro_args =
      if kv_args === [] do
        ""
      else
        "(#{kv_args_str})"
      end

    """
    % #{length(kv_args)} elements
    -define(#{build_name}#{maybe_macro_args}, #{wrapped_str}).

    -define(#{match_name}#{maybe_macro_args}, #{wrapped_str}).
    """
  end

  defp macro_build_name(nr_of_keys) do
    "?LEAF#{nr_of_keys}"
  end

  defp macro_match_name(nr_of_keys) do
    "?LEAF#{nr_of_keys}_MATCH"
  end

  defp macro_wrap_type(nr_of_keys) do
    cond do
      nr_of_keys === 0 ->
        :empty

      nr_of_keys === 1 ->
        :improper_list

      nr_of_keys >= 2 ->
        :tuple
    end
  end

  defp macro_wrap(type, kv_args) do
    case type do
      :empty ->
        "leaf0"

      :improper_list ->
        [k_arg, v_arg] = kv_args
        "[#{k_arg} | #{v_arg}]"

      :tuple ->
        "{#{Enum.join(kv_args, ", ")}}"
    end
  end

  defp kv_args(nr_of_keys) do
    key_args(nr_of_keys) ++ value_args(nr_of_keys)
  end

  defp key_args(nr_of_keys) do
    Enum.map(1..nr_of_keys//1, &("K#{&1}"))
  end

  defp value_args(nr_of_keys) do
    Enum.map(1..nr_of_keys//1, &("V#{&1}"))
  end

  #### Get

  defp get_root_definitions(max_root_keys) do
    [
      Enum.map_join(1..max_root_keys//1, ";", &get_definition("get_root", &1)),
      ";",
      fallback_definition("get_root", "get", ["Key", "Node"]),
      "."
    ]
  end

  defp get_definitions(max_root_keys, max_keys) do
    [
      Enum.map_join((max_root_keys + 1)..max_keys//1, ";", &get_definition("get", &1)),
      "."
    ]
  end

  defp get_definition(name, nr_of_keys) do
    match_name = macro_match_name(nr_of_keys)
    key_args = key_args(nr_of_keys)
    value_args = value_args(nr_of_keys)
    kv_args = key_args ++ value_args

    indexed_key_args = Enum.with_index(key_args)

    """
    #{name}(Key, #{match_name}(#{kv_args |> Enum.join(", ")})) ->
      #{generate_exact_search("Key", indexed_key_args, &handle_get_definition(&1, &2, value_args))}
    """
  end

  defp handle_get_definition(_k_arg, k_index, value_args) do
    found_v = Enum.at(value_args, k_index)
    found_v
  end

  #### Insert

  defp insert_root_definitions(max_root_keys, max_keys) do
    [
      Enum.map_join(1..max_root_keys//1, ";", &insert_definition("insert_root", &1, max_keys)),
      ";",
      fallback_definition("insert_root", "insert", ["Key", "ValueEval", "ValueWrap", "Node"]),
      "."
    ]
  end

  defp insert_definitions(max_root_keys, max_keys) do
    [
      Enum.map_join((max_root_keys + 1)..max_keys//1, ";", &insert_definition("insert", &1, max_keys)),
      "."
    ]
  end

  defp insert_definition(name, nr_of_keys, max_keys) do
    match_name = macro_match_name(nr_of_keys)
    key_args = key_args(nr_of_keys)
    value_args = value_args(nr_of_keys)
    kv_args = key_args ++ value_args

    indexed_key_args = Enum.with_index(key_args)

    """
    #{name}(Key, ValueEval, ValueWrap, #{match_name}(#{kv_args |> Enum.join(", ")})) ->
      #{generate_gap_search("Key", indexed_key_args, &handle_insert_definition(&1, key_args, value_args, max_keys))}
    """
  end

  defp handle_insert_definition(k_index, key_args, value_args, max_keys) do
    expanded_keys = List.insert_at(key_args, k_index, "Key")
    expanded_values = List.insert_at(value_args, k_index, "Value")
    new_size = length(expanded_keys)

    if new_size > max_keys do
      mid_index = div(max_keys, 2)

      {left_keys, [mid_key | right_keys]} = Enum.split(expanded_keys, mid_index)
      {left_values, [mid_value | right_values]} = Enum.split(expanded_values, mid_index)

      left_kv_str = Enum.join(left_keys ++ left_values, ", ")
      right_kv_str = Enum.join(right_keys ++ right_values, ", ")

      """
      Value = eval_insert_value(ValueEval, ValueWrap),
      Left = #{macro_build_name(mid_index)}(#{left_kv_str}),
      Right = #{macro_build_name(mid_index)}(#{right_kv_str}),
      {split, #{mid_key}, #{mid_value}, Left, Right}
      """
    else
      expanded_kv_str = Enum.join(expanded_keys ++ expanded_values, ", ")
      """
      Value = eval_insert_value(ValueEval, ValueWrap),
      #{macro_build_name(new_size)}(#{expanded_kv_str})
      """
    end
  end

  ## exact search

  defp generate_exact_search(target_k, indexed_key_args, handler_fun) do
    case indexed_key_args do
      [pair_a] ->
        generate_exact_search1(target_k, pair_a, handler_fun)

      [pair_a, pair_b] ->
        generate_exact_search2(target_k, pair_a, pair_b, handler_fun)

      multiple when length(multiple) >= 3 ->
        {left_pairs, [{mid_k, mid_index} | right_pairs]} = Enum.split(indexed_key_args, div(length(indexed_key_args), 2))

        """
        if
          #{target_k} < #{mid_k} ->
            #{generate_exact_search(target_k, left_pairs, handler_fun)};

          #{target_k} > #{mid_k} ->
            #{generate_exact_search(target_k, right_pairs, handler_fun)};

          true ->
            #{handler_fun.(mid_k, mid_index)}
        end
        """
    end
  end

  defp generate_exact_search1(target_k, {k, index}, handler_fun) do
    """
    if
      #{target_k} == #{k} ->
        #{handler_fun.(k, index)};
      %
      true ->
        error({badkey, #{target_k}})
    end
    """
  end

  defp generate_exact_search2(target_k, {k1, index1}, {k2, index2}, handler_fun) do
    """
    if
      #{target_k} == #{k1} ->
        #{handler_fun.(k1, index1)};
      %
      #{target_k} == #{k2} ->
        #{handler_fun.(k2, index2)};
      %
      true ->
        error({badkey, #{target_k}})
    end
    """
  end

  ## gap search

  defp generate_gap_search(target_k, indexed_key_args, handler_fun) do
    case indexed_key_args do
      [] ->
        handler_fun.(0)

      [pair_a] ->
        generate_gap_search1(target_k, pair_a, handler_fun)

      multiple when length(multiple) >= 2 ->
        {left_pairs, [{mid_k, _mid_index} | right_pairs]} = Enum.split(indexed_key_args, div(length(indexed_key_args), 2))

        """
        if
          #{target_k} < #{mid_k} ->
            #{generate_gap_search(target_k, left_pairs, handler_fun)};
          %
          #{target_k} > #{mid_k} ->
            #{generate_gap_search(target_k, right_pairs, handler_fun)};
          %
          true ->
            error({key_exists, #{target_k}})
        end
        """
    end
  end

  defp generate_gap_search1(target_k, {k, index}, handler_fun) do
    """
    if
      #{target_k} < #{k} ->
        #{handler_fun.(index)};
      %
      #{target_k} > #{k} ->
        #{handler_fun.(index + 1)};
      %
      true ->
        error({key_exists, #{target_k}})
    end
    """
  end

  defp fallback_definition(name, fallback_name, args) do
    args_str = Enum.join(args, ", ")

    """
    #{name}(#{args_str}) ->
        #{fallback_name}(#{args_str})
    """
  end
end

[order_arg, mode_arg] = System.argv()
btree_order = order_arg |> String.to_integer()
mode = String.to_existing_atom(mode_arg)

Generator.run(btree_order, mode)
