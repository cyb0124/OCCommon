#include "Actions.h"

namespace Actions {
  void Print::dump(nlohmann::json &j) {
    j = {{"op", "print"}, {"color", color}};
    j["text"] = std::move(text);
    if (beep) j["beep"] = *beep;
  }

  void List::dump(nlohmann::json &j) {
    j = {{"op", "list"}, {"side", side}};
    j["inv"] = std::move(inv);
  }

  void List::onResponse(nlohmann::json &j) {
    std::vector<SharedItemStack> items(j.size());
    for (size_t i = 0; i < items.size(); ++i) {
      auto &ji = j.at(i);
      if (!ji.is_string())
        items[i] = parseItemStack(ji);
    }
    onResponse(items);
  }

  void ListXN::dump(nlohmann::json &j) {
    List::dump(j);
    j["x"] = x;
    j["y"] = y;
    j["z"] = z;
  }

  void ListME::dump(nlohmann::json &j) {
    j = {{"op", "listME"}};
    j["inv"] = std::move(inv);
  }

  void ListME::onResponse(nlohmann::json &j) {
    std::vector<SharedItemStack> items(j.size());
    for (size_t i = 0; i < items.size(); ++i)
      items[i] = parseItemStack(j.at(i));
    onResponse(items);
  }

  void XferME::dump(nlohmann::json &j) {
    j = {{"op", "xferME"}, {"fromSide", fromSide}, {"toSide", toSide}, {"slot", slot}};
    j["filter"] = std::move(filter);
    j["inv"] = std::move(inv);
    j["me"] = std::move(me);
  }

  void Call::dump(nlohmann::json &j) {
    j = {{"op", "call"}};
    j["args"] = std::move(args);
    j["inv"] = std::move(inv);
  }
}
