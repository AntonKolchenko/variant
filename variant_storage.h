#include "helper_classes.h"

template <bool Tr, typename... Args>
union storage {
  constexpr storage() {}
  constexpr storage(empty_class e) {}
  template <std::size_t I, typename... Types>
  constexpr storage(in_place_index_t<I>, Types&&... args) {}
  template <typename... Types>
  constexpr storage(in_place_index_t<0>, Types&&... args) {}
  constexpr void destroy(std::size_t index) {}
  constexpr ~storage() = default;
};

template <typename Head, typename... Tail>
union storage<true, Head, Tail...> {
  Head head;
  storage<true, Tail...> tail;

  constexpr storage() : head() {}
  constexpr storage(empty_class e) : tail(e) {}

  template <std::size_t I, typename... Args>
  constexpr storage(in_place_index_t<I>, Args&&... args) : tail(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr storage(in_place_index_t<0>, Args&&... args) : head(std::forward<Args>(args)...) {}

  constexpr ~storage() = default;

  constexpr void destroy(std::size_t index) {
    if (index == 0) {
      head.~Head();
    } else {
      tail.destroy(index - 1);
    }
  }

  template <std::size_t I>
  constexpr auto& get(in_place_index_t<I>) {
    if constexpr (I == 0) {
      return head;
    } else {
      return tail.get(in_place_index<I - 1>);
    }
  }

  template <std::size_t I>
  constexpr auto const& get(in_place_index_t<I>) const {
    if constexpr (I == 0) {
      return head;
    } else {
      return tail.get(in_place_index<I - 1>);
    }
  }
  template <std::size_t I, typename... Args>
  constexpr void put(in_place_index_t<I>, Args&&... args) {
    if constexpr (I == 0) {
      new (this) Head(std::forward<Args>(args)...);
    } else {
      tail.put(in_place_index<I - 1>, std::forward<Args>(args)...);
    }
  }
};

template <bool Tr, typename Head, typename... Tail>
union storage<Tr, Head, Tail...> {
  Head head;
  storage<false, Tail...> tail;

  constexpr storage() : head() {}
  constexpr storage(empty_class e) : tail(e) {}

  template <std::size_t I, typename... Args>
  constexpr storage(in_place_index_t<I>, Args&&... args) : tail(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr storage(in_place_index_t<0>, Args&&... args) : head(std::forward<Args>(args)...) {}

  constexpr void destroy(std::size_t index) {
    if (index == 0) {
      head.~Head();
    } else {
      tail.destroy(index - 1);
    }
  }

  constexpr ~storage() {}

  template <std::size_t I>
  constexpr auto const& get(in_place_index_t<I>) const {
    if constexpr (I == 0) {
      return head;
    } else {
      return tail.get(in_place_index<I - 1>);
    }
  }

  template <std::size_t I>
  constexpr auto& get(in_place_index_t<I>) {
    if constexpr (I == 0) {
      return head;
    } else {
      return tail.get(in_place_index<I - 1>);
    }
  }

  template <std::size_t I, typename... Args>
  constexpr void put(in_place_index_t<I>, Args&&... args) {
    if constexpr (I == 0) {
      new (this) Head(std::forward<Args>(args)...);
    } else {
      tail.put(in_place_index<I - 1>, std::forward<Args>(args)...);
    }
  }
};
