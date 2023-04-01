#pragma once

#include <exception>
#include <type_traits>

template <typename... Args>
struct variant;

template <typename T>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<T const> : std::integral_constant<std::size_t, variant_size<T>::value> {};

template <typename T>
struct variant_size<T volatile> : std::integral_constant<std::size_t, variant_size<T>::value> {};

template <typename T>
inline constexpr std::size_t variant_size_v = variant_size<T>::value;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename... Types>
struct type_by_index_imp;

template <std::size_t Index, typename Head, typename... Tail>
struct type_by_index_imp<Index, Head, Tail...> {
  using value = typename type_by_index_imp<Index - 1, Tail...>::value;
};
template <typename Head, typename... Tail>
struct type_by_index_imp<0, Head, Tail...> {
  using value = Head;
};

template <std::size_t Index, typename... Types>
requires(Index < sizeof...(Types)) using type_by_index_v = typename type_by_index_imp<Index, Types...>::value;

template <std::size_t I, typename Head, typename... Tail>
struct variant_alternative<I, variant<Head, Tail...>> {
  using type = type_by_index_v<I, Head, Tail...>;
};

template <std::size_t I, typename T>
struct variant_alternative<I, const T> : variant_alternative<I, T> {
  using type = typename variant_alternative<I, T>::type const;
};

template <std::size_t I, typename T>
struct variant_alternative<I, volatile T> : variant_alternative<I, T> {
  using type = typename variant_alternative<I, T>::type volatile;
};

template <std::size_t I, typename T>
struct variant_alternative<I, const volatile T> : variant_alternative<I, T> {
  using type = typename variant_alternative<I, T>::type const volatile;
};

template <size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template <typename... Types>
concept all_is_copy_constructible_v = (std::is_copy_constructible_v<Types> && ...);

template <typename... Types>
concept all_is_trivially_copy_constructible_v = (std::is_trivially_copy_constructible_v<Types> && ...);

template <typename... Types>
concept all_is_trivially_move_constructible_v = (std::is_trivially_move_constructible_v<Types> && ...);

template <typename... Types>
concept all_is_move_constructible_v = (std::is_move_constructible_v<Types> && ...);

template <typename... Types>
concept all_is_nothrow_move_constructible_v = (std::is_nothrow_move_constructible_v<Types> && ...);

template <typename... Types>
concept all_is_nothrow_copy_constructible_v = (std::is_nothrow_copy_constructible_v<Types> && ...);

template <typename... Types>
concept all_is_nothrow_move_assignable_v = (std::is_nothrow_move_assignable_v<Types> && ...);

template <typename... Types>
concept all_is_nothrow_copy_assignable_v = (std::is_nothrow_copy_assignable_v<Types> && ...);

template <typename... Types>
concept all_is_trivially_destructible_v = (std::is_trivially_destructible_v<Types> && ...);

template <typename... Types>
concept all_is_trivially_copy_assignable_v = (std::is_trivially_copy_assignable_v<Types> && ...);

template <typename... Types>
concept all_is_move_assignable_v = (std::is_move_assignable_v<Types> && ...);

template <typename... Types>
concept all_is_trivially_move_assignable_v = (std::is_trivially_move_assignable_v<Types> && ...);

template <typename... Types>
concept all_is_copy_assignable_v = (std::is_copy_assignable_v<Types> && ...);

template <typename... Args>
struct count {
  static constexpr std::size_t value = 0;
};

template <typename T, typename Head, typename... Tail>
struct count<T, Head, Tail...> {
  constexpr static std::size_t value = std::is_same_v<T, Head> + count<T, Tail...>::value;
};

template <typename T, typename... Types>
concept exactly_once = (count<T, Types...>::value == 1);

template <typename T, typename... Args>
struct index_by_type_impl {
  static constexpr std::size_t value = 0;
};

template <typename T, typename Head, typename... Tail>
struct index_by_type_impl<T, Head, Tail...> {
  static constexpr std::size_t value = std::is_same_v<T, Head> ? 0 : index_by_type_impl<T, Tail...>::value + 1;
};

template <typename T, typename... Args>
inline constexpr std::size_t index_by_type_v = index_by_type_impl<T, Args...>::value;

struct in_place_t {
  explicit in_place_t() = default;
};

inline constexpr in_place_t in_place{};

template <typename T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};
template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

inline constexpr std::size_t variant_npos = -1;

template <typename T, typename Type>
concept possible_create = requires(T && t) {
  Type{std::forward<T>(t)};
};

template <typename T, typename Type>
concept possible_fuction = possible_create<T, Type[]>;

struct bad_variant_access : std::exception {
  bad_variant_access() noexcept {}

  const char* what() const noexcept override {
    return response;
  }

private:
  const char* response = "bad_variant_access";
};

template <typename... Args>
concept is_all_trivially_destructible_v = (std::is_trivially_destructible_v<Args> && ...);

template <std::size_t N, typename T, typename Type, typename... Types>
struct get_index_to_construct : get_index_to_construct<N + 1, T, Types...> {
  using get_index_to_construct<N + 1, T, Types...>::get_index;
  constexpr std::integral_constant<std::size_t, N> get_index(Type) requires(possible_fuction<T, Type>);
};

template <std::size_t N, typename T, typename Type>
struct get_index_to_construct<N, T, Type> {
  constexpr std::integral_constant<std::size_t, N> get_index(Type) requires(possible_fuction<T, Type>);
};

template <typename T, typename... Types>
using ind_to_construct = decltype(get_index_to_construct<0, T, Types...>().get_index(std::declval<T>()));

struct empty_class {};
