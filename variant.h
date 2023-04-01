#pragma once

#include <iostream>

#include <type_traits>

#include "helper_classes.h"
#include "visit.h"

#include "variant_base.h"

template <typename... Args>
struct variant : base<is_all_trivially_destructible_v<Args...>, Args...> {

  using base_class = base<is_all_trivially_destructible_v<Args...>, Args...>;

  constexpr ~variant() = default;

  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<type_by_index_v<0, Args...>>)
      requires(std::is_default_constructible_v<type_by_index_v<0, Args...>>)
      : base_class(in_place_index<0>) {}

  constexpr variant(variant const& other)
      requires(all_is_copy_constructible_v<Args...>&& all_is_trivially_copy_constructible_v<Args...>) = default;

  constexpr variant(variant const& other) noexcept(all_is_nothrow_copy_constructible_v<Args...>)
      requires(all_is_copy_constructible_v<Args...>)
      : base_class(empty_class{}) {
    if (!other.valueless_by_exception()) {
      visit_by_index(
          [this]<std::size_t N>(in_place_index_t<N>, variant const& other_v) {
            this->m_storage.put(in_place_index<N>, get<N>(other_v));
          },
          other);
      this->m_index = other.index();
    }
  }

  constexpr variant(variant&& other) noexcept(all_is_nothrow_move_constructible_v<Args...>)
      requires(all_is_move_constructible_v<Args...>&& all_is_trivially_move_constructible_v<Args...>) = default;

  constexpr variant(variant&& other) noexcept(all_is_nothrow_move_constructible_v<Args...>)
      requires(all_is_move_constructible_v<Args...>)
      : base_class(empty_class{}) {
    if (!other.valueless_by_exception()) {
      visit_by_index(
          [this]<std::size_t N>(in_place_index_t<N>, variant&& rhs) {
            this->m_storage.put(in_place_index<N>, std::move(get<N>(rhs)));
          },
          std::move(other));
      this->m_index = other.index();
    }
  }

  template <typename T, std::size_t I = ind_to_construct<T, Args...>::value>
  constexpr variant(T&& t) noexcept(std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, T>)
      requires(I < variant_size_v<variant>)
      : base_class(in_place_index<I>, std::forward<T>(t)) {}

  template <typename T, typename... Types, std::size_t I = index_by_type_v<T, Args...>>
  constexpr explicit variant(in_place_type_t<T>, Types&&... types) noexcept(
      std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, Types...>)
      requires(exactly_once<T, Args...>&& std::is_constructible_v<T, Types...>)
      : base_class(in_place_index<I>, std::forward<Types>(types)...) {}

  template <std::size_t I, typename... Types>
  constexpr explicit variant(in_place_index_t<I>, Types&&... types) noexcept(
      std::is_nothrow_constructible_v<variant_alternative_t<I, variant>, Types...>)
      requires((I < sizeof...(Args)) && std::is_constructible_v<type_by_index_v<I, Args...>, Types...>)
      : base_class(in_place_index<I>, std::forward<Types>(types)...) {}

  constexpr variant& operator=(variant const& rhs)
      requires(all_is_copy_constructible_v<Args...>&& all_is_copy_assignable_v<Args...>&&
                   all_is_trivially_destructible_v<Args...>&& all_is_trivially_copy_assignable_v<Args...>&&
                       all_is_trivially_copy_constructible_v<Args...>) = default;

  constexpr variant& operator=(variant const& rhs) noexcept(
      all_is_nothrow_copy_constructible_v<Args...>&& all_is_nothrow_copy_assignable_v<Args...>)
      requires(all_is_copy_constructible_v<Args...>&& all_is_copy_assignable_v<Args...>) {

    if (this->index() != rhs.index()) {
      this->operator=(variant(rhs));
    } else if (!this->valueless_by_exception()) {
      visit_by_index(
          [this]<std::size_t I>(in_place_index_t<I>, variant const& other) { get<I>(*this) = get<I>(other); }, rhs);
    }

    this->m_index = rhs.index();

    return *this;
  }

  constexpr variant& operator=(variant&& rhs) noexcept(((std::is_nothrow_move_constructible_v<Args> &&
                                                         std::is_nothrow_move_assignable_v<Args>)&&...))
      requires(all_is_move_constructible_v<Args...>&& all_is_move_assignable_v<Args...>&&
                   all_is_trivially_move_constructible_v<Args...>&& all_is_trivially_move_assignable_v<Args...>&&
                       all_is_trivially_destructible_v<Args...>) = default;

  constexpr variant& operator=(variant&& rhs) noexcept(((std::is_nothrow_move_constructible_v<Args> &&
                                                         std::is_nothrow_move_assignable_v<Args>)&&...))
      requires(all_is_move_constructible_v<Args...>&& all_is_move_assignable_v<Args...>) {

    visit_by_index(
        [this]<std::size_t I>(in_place_index_t<I>, variant&& other) {
          if (this->index() != other.index()) {
            this->destroy();
            this->m_storage.put(in_place_index<I>, std::move(get<I>(other)));
          } else {
            if (!this->valueless_by_exception()) {
              get<I>(*this) = std::move(get<I>(other));
            }
          }
        },
        std::move(rhs));

    this->m_index = rhs.index();
    return *this;
  }

  template <typename T, std::size_t I = ind_to_construct<T, Args...>::value, typename T_j = type_by_index_v<I, Args...>>
  constexpr variant&
  operator=(T&& t) noexcept(std::is_nothrow_assignable_v<T_j&, T>&& std::is_nothrow_constructible_v<T_j, T>)
      requires(I < variant_size_v<variant>) {

    if (this->index() == I) {
      get<I>(*this) = std::forward<T>(t);
    } else {
      this->emplace<I>(variant_alternative_t<I, variant>(std::forward<T>(t)));
    }
    this->m_index = I;
    return *this;
  }

  template <class T, class... Types>
  constexpr T& emplace(Types&&... types) requires(std::is_constructible_v<T, Types...>&& exactly_once<T, Args...>) {
    return this->emplace<index_by_type_v<T, Args...>>(std::forward<Types>(types)...);
  }

  template <std::size_t I, class... Types>
  constexpr variant_alternative_t<I, variant>& emplace(Types&&... types)
      requires(std::is_constructible_v<variant_alternative_t<I, variant>, Types...>&& I < variant_size_v<variant>) {
    this->destroy();
    this->put(in_place_index<I>, std::forward<Types>(types)...);
    this->m_index = I;
    return this->m_storage.get(in_place_index<I>);
  }

  constexpr void swap(variant& rhs) noexcept(((std::is_nothrow_move_constructible_v<Args> &&
                                               std::is_nothrow_swappable_v<Args>)&&...)) {
    if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
      return;
    }
    using std::swap;
    if (this->valueless_by_exception()) {
      visit_by_index(
          [this]<std::size_t I>(in_place_index_t<I>, variant& other) {
            this->m_storage.put(in_place_index<I>, std::move(get<I>(other)));
          },
          rhs);
      swap(this->m_index, rhs.m_index);
      return;
    }

    if (rhs.valueless_by_exception()) {
      rhs.swap(*this);
      return;
    }

    if (this->index() == rhs.index()) {
      visit_by_index([this]<std::size_t I>(in_place_index_t<I>, variant& other) { swap(get<I>(*this), get<I>(other)); },
                     rhs);
      return;
    }

    std::swap(*this, rhs);
  }
};

template <std::size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  if (pv != nullptr && pv->index() == I) {
    return std::addressof(get<I>(*pv));
  }
  return nullptr;
}

template <std::size_t I, class... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(variant<Types...> const* pv) noexcept {
  if (pv != nullptr && pv->index() == I) {
    return std::addressof(get<I>(*pv));
  }
  return nullptr;
}

template <class T, class... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  return get_if<index_by_type_v<T, Types...>>(pv);
}

template <class T, class... Types>
constexpr std::add_pointer_t<const T> get_if(variant<Types...> const* pv) noexcept {
  return get_if<index_by_type_v<T, Types...>>(pv);
}

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
  if (v.index() == I) {
    return v.m_storage.get(in_place_index<I>);
  }
  throw bad_variant_access();
}
template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
  return std::move(get<I>(v));
}

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>> const& get(variant<Types...> const& v) {
  if (v.index() == I) {
    return v.m_storage.get(in_place_index<I>);
  }
  throw bad_variant_access();
}
template <std::size_t I, class... Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(variant<Types...> const&& v) {
  return std::move(get<I>(v));
}

template <class T, class... Types, std::size_t I = index_by_type_v<T, Types...>>
constexpr T& get(variant<Types...>& v) {
  return get<I>(v);
}
template <class T, class... Types, std::size_t I>
constexpr T&& get(variant<Types...>&& v) {
  return get<I>(std::move(v));
}
template <class T, class... Types, std::size_t I>
constexpr const T& get(variant<Types...> const& v) {
  return get<I>(v);
}
template <class T, class... Types, std::size_t I>
constexpr const T&& get(variant<Types...> const&& v) {
  return get<I>(std::move(v));
}

template <class T, class... Types>
constexpr bool holds_alternative(variant<Types...> const& v) noexcept {
  return index_by_type_v<T, Types...> == v.index();
}

template <class... Types>
constexpr bool operator==(variant<Types...> const& v, variant<Types...> const& w) {
  if (v.index() != w.index()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  return visit_by_index(
      [&v]<std::size_t I>(in_place_index_t<I>, variant<Types...> const& w) -> bool { return get<I>(v) == get<I>(w); },
      w);
}

template <class... Types>
bool operator!=(variant<Types...> const& v, variant<Types...> const& w) {
  return !(v == w);
}

template <class... Types>
constexpr bool operator<(variant<Types...> const& v, variant<Types...> const& w) {
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit_by_index(
      [&v]<std::size_t I>(in_place_index_t<I>, variant<Types...> const& w) -> bool { return get<I>(v) < get<I>(w); },
      w);
}
template <class... Types>
constexpr bool operator>(variant<Types...> const& v, variant<Types...> const& w) {
  return w < v;
}
template <class... Types>
constexpr bool operator<=(variant<Types...> const& v, variant<Types...> const& w) {
  if (v.valueless_by_exception()) {
    return true;
  }
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit_by_index(
      [&v]<std::size_t I>(in_place_index_t<I>, variant<Types...> const& w) -> bool { return get<I>(v) <= get<I>(w); },
      w);
}

template <class... Types>
constexpr bool operator>=(variant<Types...> const& v, variant<Types...> const& w) {
  return w <= v;
}

template <class... Types>
constexpr void swap(variant<Types...>& lhs, variant<Types...>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
  lhs.swap(rhs);
}
