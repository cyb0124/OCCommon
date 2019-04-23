#include <cstdint>
#include <random>
#include <array>
#include <iostream>

static constexpr int farmSize = 7;
static constexpr int sprinklerRange = 4;
static constexpr int maxSprinklers = 9;

static constexpr int nGenTotal = 512;
static constexpr int popSize = 4096;
static constexpr int pressure = 4;

static std::mt19937_64 random(std::random_device{ }());

struct Fitness {
  int nWater = 0;
  int nSprinkler = 0;
};

struct Layout {
  // true = sprinkler
  std::array<bool, farmSize * farmSize> data;
  bool operator()(int x, int y) const { return data[x * farmSize + y]; }
  bool &operator()(int x, int y) { return data[x * farmSize + y]; }
  Fitness fitness() const {
    Fitness result;
    for (int x = 0; x < farmSize; ++x) {
      for (int y = 0; y < farmSize; ++y) {
        if ((*this)(x, y)) {
          ++result.nSprinkler;
          int xMin = std::max(x - sprinklerRange, 0);
          int xMax = std::min(x + sprinklerRange, farmSize - 1);
          int yMin = std::max(y - sprinklerRange, 0);
          int yMax = std::min(y + sprinklerRange, farmSize - 1);
          for (int x = xMin; x <= xMax; ++x) {
            for (int y = yMin; y <= yMax; ++y) {
              result.nWater += !(*this)(x, y);
            }
          }
        }
      }
    }
    return result;
  }

  void randomize() {
    for (auto &i : data) i = std::bernoulli_distribution()(random);
  }
};

struct Individual {
  Layout layout;
  Fitness fitness;
  bool operator>(const Individual &other) const {
    bool thisExceed = fitness.nSprinkler > maxSprinklers;
    bool otherExceed = other.fitness.nSprinkler > maxSprinklers;
    if (!thisExceed && otherExceed) return true;
    if (thisExceed && !otherExceed) return false;
    if (!thisExceed || !otherExceed || fitness.nSprinkler == other.fitness.nSprinkler) {
      if (fitness.nWater > other.fitness.nWater) return true;
      if (fitness.nWater < other.fitness.nWater) return false;
    }
    return fitness.nSprinkler < other.fitness.nSprinkler;
  }
  bool operator<(const Individual &other) const { return !(*this > other); }
  void print() const {
    std::cout << "nWater=" << fitness.nWater << ", nSprinkler=" << fitness.nSprinkler << ":\n";
    int count = 0;
    for (int y = 0; y < farmSize; ++y) {
      for (int x = 0; x < farmSize; ++x) {
        bool now = layout(x, y);
        std::cout << (now ? "X" : "O");
        count += now;
      }
      std::cout << "\n";
    }
    std::cout << std::flush;
  }
};

static std::vector<Individual> popA(popSize), popB(popSize);

void initialize() {
  for (auto &i : popA) {
    i.layout.randomize();
    i.fitness = i.layout.fitness();
  }
}

Individual &tournament() {
  Individual *best = &popA[std::uniform_int_distribution<>(0, popSize - 1)(random)];
  for (int i = 1; i < pressure; ++i) {
    Individual *now = &popA[std::uniform_int_distribution<>(0, popSize - 1)(random)];
    if (*now > *best) best = now;
  }
  return *best;
}

void crossover(Layout &x, Layout &y) {
  for (int i = 0; i < farmSize * farmSize; ++i) {
    if (std::bernoulli_distribution()(random)) {
      std::swap(x.data[i], y.data[i]);
    }
  }
}

void mutate(Layout &x) {
  for (bool &i : x.data) {
    if (!std::uniform_int_distribution<>(0, farmSize * farmSize)(random)) {
      i = !i;
    }
  }
}

void generation() {
  for (int i = 0; i < popSize; i += 2) {
    auto &c1 = popB[i + 0], &c2 = popB[i + 1];
    c1 = tournament(); c2 = tournament();
    crossover(c1.layout, c2.layout);
    mutate(c1.layout); mutate(c2.layout);
    c1.fitness = c1.layout.fitness();
    c2.fitness = c2.layout.fitness();
  }
  swap(popA, popB);
}

int main() {
  initialize();
  for (int i = 0; i < nGenTotal; ++i) {
    generation();
    std::cout << "generation " << i << ":\n";
    std::max_element(popB.begin(), popB.end())->print();
  }
  system("pause");
}
