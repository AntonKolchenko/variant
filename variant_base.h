#include "variant_storage.h"

template <bool Tr, typename... Args>
struct base {
  std::size_t m_index = 0;
  storage<false, Args...> m_storage;

  constexpr base(empty_class e) : m_storage(e), m_index(0) {}

  template <std::size_t I, typename... Types>
  constexpr void put(in_place_index_t<I>, Types&&... types) {
    m_storage.put(in_place_index<I>, std::forward<Types>(types)...);
  }

  template <std::size_t I, typename... Types>
  constexpr base(in_place_index_t<I>, Types&&... types)
      : m_storage(in_place_index<I>, std::forward<Types>(types)...), m_index(I) {}

  constexpr bool valueless_by_exception() const noexcept {
    return m_index == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return m_index;
  }

  constexpr void destroy() {
    if (m_index != variant_npos) {
      m_storage.destroy(m_index);
    }
    m_index = variant_npos;
  }

  constexpr ~base() {
    destroy();
  }
};

template <typename... Args>
struct base<true, Args...> {
  std::size_t m_index = 0;
  storage<true, Args...> m_storage;

  constexpr base(empty_class e) : m_storage(e), m_index(0) {}

  template <std::size_t I, typename... Types>
  constexpr base(in_place_index_t<I>, Types&&... types)
      : m_storage(in_place_index<I>, std::forward<Types>(types)...), m_index(I) {}

  template <std::size_t I, typename... Types>
  constexpr void put(in_place_index_t<I>, Types&&... types) {
    m_storage.put(in_place_index<I>, std::forward<Types>(types)...);
  }

  constexpr bool valueless_by_exception() const noexcept {
    return m_index == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return m_index;
  }

  constexpr void destroy() {
    m_index = variant_npos;
  }

  constexpr ~base() = default;
};
