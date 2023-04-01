#pragma once
#include "helper_classes.h"

#include <array>
#include <functional>
#include <type_traits>
#include <utility>

template <typename Visitor, typename... Variants, std::size_t... Indexes>
constexpr auto get_matrix_impl(std::index_sequence<Indexes...>) noexcept {
  struct dispatcher {
    static constexpr decltype(auto) dispatch(Visitor&& v, Variants&&... var) {
      return (std::forward<Visitor>(v))(get<Indexes>(std::forward<Variants>(var))...);
    }
  };
  return &dispatcher::dispatch;
}

template <typename Visitor, typename... Variants, std::size_t... IS, std::size_t... JS, typename... LS>
constexpr auto get_matrix_impl(std::index_sequence<IS...>, std::index_sequence<JS...>, LS... ls) noexcept {
  return std::array{get_matrix_impl<Visitor, Variants...>(std::index_sequence<IS..., JS>(), ls...)...};
}

template <typename Visitor, typename... Variants>
constexpr auto get_matrix() noexcept {
  return get_matrix_impl<Visitor, Variants...>(std::index_sequence<>(),
                                               std::make_index_sequence<variant_size_v<std::decay_t<Variants>>>()...);
}

template <typename Visitor, typename Variant, std::size_t Index>
constexpr auto get_array_impl_impl(std::index_sequence<Index>) noexcept {
  struct dispatcher {
    static constexpr decltype(auto) dispatch(Visitor&& v, Variant&& var) {
      return std::forward<Visitor>(v)(in_place_index<Index>, std::forward<Variant>(var));
    }
  };
  return &dispatcher::dispatch;
}

template <typename Visitor, typename Variant, std::size_t... Indexes>
constexpr auto get_array_impl(std::index_sequence<Indexes...>) noexcept {
  return std::array{get_array_impl_impl<Visitor, Variant>(std::index_sequence<Indexes>())...};
}

template <typename Visitor, typename Variant>
constexpr auto get_array() noexcept {
  return get_array_impl<Visitor, Variant>(std::make_index_sequence<variant_size_v<std::decay_t<Variant>>>());
}

template <typename F>
constexpr decltype(auto) at(F&& func) {
  return std::forward<F>(func);
}

template <typename Matrix, typename... Indexes>
constexpr decltype(auto) at(Matrix&& matrix, std::size_t index, Indexes... others) {
  return at(std::forward<Matrix>(matrix)[index], others...);
}

template <typename Visitor, typename Variant>
constexpr decltype(auto) visit_by_index(Visitor&& visitor, Variant&& variant) {
  return at(get_array<Visitor, Variant>(), variant.index())(std::forward<Visitor>(visitor),
                                                            std::forward<Variant>(variant));
}

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& visitor, Variants&&... variants) {
  if ((variants.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  return at(get_matrix<Visitor, Variants...>(), variants.index()...)(std::forward<Visitor>(visitor),
                                                                     std::forward<Variants>(variants)...);
}