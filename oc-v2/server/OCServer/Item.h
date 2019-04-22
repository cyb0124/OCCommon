#pragma once
#include <memory>
#include "json.hpp"

struct Item {
  std::string name, label;
  int damage, maxDamage, maxSize;
  bool hasTag;
  nlohmann::json others;

  bool operator==(const Item &other) const;
  bool operator!=(const Item &other) const;
};

using SharedItem = std::shared_ptr<Item>;

struct ItemStack {
  SharedItem item;
  int size;
};

using SharedItemStack = std::shared_ptr<ItemStack>;

SharedItemStack parseItemStack(nlohmann::json&);
