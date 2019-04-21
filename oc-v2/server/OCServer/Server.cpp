#include <iostream>
#include "Server.h"
#include "WeakCallback.h"

Server::Server(IOEnv &io)
  :io(io), acceptor(io.io, boost::asio::ip::tcp::v4()) {}

void Server::init(uint16_t port) {
  acceptor.set_option(boost::asio::ip::tcp::socket::reuse_address(true));
  acceptor.bind({boost::asio::ip::address_v4::any(), port});
  acceptor.listen();
  startAccept();
}

void Server::startAccept() {
  auto socket(std::make_shared<boost::asio::ip::tcp::socket>(io.io));
  acceptor.async_accept(*socket, makeWeakCallback(weak_from_this(), [this, socket](
    const boost::system::error_code &error
  ) {
    if (error)
      throw boost::system::system_error(error, "async_accept failed");
    startAccept();
    clients.emplace_front(std::make_shared<Client>(*this, std::move(*socket)))->init(clients.begin());
  }));
}

void Server::removeClient(Client &c) {
  if (c.getLogin())
    logins.erase(*c.getLogin());
  clients.erase(c.getItr());
}

Client::Client(Server &s, boost::asio::ip::tcp::socket &&socket)
  :s(s), socket(std::move(socket)) {}

void Client::init(const Itr &x) {
  itr = x;
  const auto &host = socket.remote_endpoint();
  logHeader = host.address().to_string() + ":" + std::to_string(host.port());
  log("connected");
  readLength();
}

Client::~Client() {
  log("disconnected");
}

void Client::log(const std::string &message) {
  std::cout << logHeader << ": " << message << std::endl;
}

void Client::readLength() {
  auto buffer(std::make_shared<std::array<uint8_t, 3>>());
  boost::asio::async_read(socket, boost::asio::buffer(*buffer), makeWeakCallback(weak_from_this(), [this, buffer](
    const boost::system::error_code &ec, size_t
  ) {
    if (ec) {
      log("error reading length: " + ec.message());
      s.removeClient(*this);
    } else {
      auto &b = *buffer;
      size_t size = static_cast<size_t>(b[0]) | static_cast<size_t>(b[1]) << 8u | static_cast<size_t>(b[2]) << 16u;
      if (size) {
        readContent(size);
      } else {
        readLength();
        onPacket(nullptr, 0);
      }
    }
  }));
}

void Client::readContent(size_t size) {
  std::shared_ptr<char[]> buffer(new char[size]);
  boost::asio::async_read(socket, boost::asio::buffer(buffer.get(), size), makeWeakCallback(weak_from_this(), [this, buffer](
    const boost::system::error_code &ec, size_t size
  ) {
    if (ec) {
      log("error reading content: " + ec.message());
      s.removeClient(*this);
    } else {
      readLength();
      onPacket(buffer.get(), size);
    }
  }));
}

void Client::onPacket(const char *data, size_t size) {
  try {
    onPacket(nlohmann::json::parse(data, data + size));
  } catch (std::exception &e) {
    log(std::string("error decoding packet: ") + e.what());
    s.removeClient(*this);
  }
}

void Client::onPacket(nlohmann::json &&j) {
  if (login) {
    // TODO:
  } else {
    login = j.get<std::string>();
    logHeader += "(" + *login + ")";
    log("logged in");
    s.updateLogin(*this);
  }
}

void Server::updateLogin(Client &c) {
  auto result(logins.emplace(*c.getLogin(), &c));
  if (!result.second) {
    Client *old = result.first->second;
    old->log("evicted");
    clients.erase(old->getItr());
    result.first->second = &c;
  }
}
