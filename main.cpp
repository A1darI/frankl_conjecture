#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <math.h>
#include <map>

using namespace std;

#define INPUT_FILE "../input.txt"

uint16_t numbersToBitSet(const vector<int>& numbers) {
    if (numbers.size() == 0) {
        return 0;
    }

    uint16_t bit_set = 0;
    size_t cur_pow = pow(2, numbers[0] - 1);
    bit_set = cur_pow;
    for (size_t i = 1; i < numbers.size(); ++i) {
        cur_pow *= pow(2, numbers[i] - numbers[i - 1]);
        bit_set += cur_pow;
    }
    return bit_set;
}
int n = 0;

class Set {
public:
    explicit Set(uint16_t number) : bit_set_(number) {
        size_ = 0;
        while (number) {
            size_ += number & 1;
            number >>= 1;
        }
    }
    Set(const vector<int>& numbers) : Set(numbersToBitSet(numbers)) {};

    auto operator<=>(const Set& other) const {
        return static_cast<int>(bit_set_) - static_cast<int>(other.bit_set_);
    }
    Set operator|(const Set& other) const {
        return Set(bit_set_ | other.bit_set_);
    }

    size_t size() const {
        return size_;
    }
    uint16_t bitSet() const {
        return bit_set_;
    }
    vector<int> getNumbers() const {
        if (bit_set_ == 0) {
            return {};
        }
        vector<int> result;
        for (size_t i = 0; i < 16; ++i) {
            if (bit_set_ & (1 << i)) {
                result.push_back(i + 1);
            }
        }
        return result;
    }
private:
    uint16_t bit_set_;
    size_t size_;
};
ostream& operator<<(ostream &os, const Set& set) {
    if (set.size() == 0) {
        os << "{}";
        return os;
    }

    os << "{";
    auto numbers = set.getNumbers();
    for (size_t i = 0; i + 1 < numbers.size(); ++i) {
        os << numbers[i] << ", ";
    }
    os << numbers.back();
    os << "}";
    return os;
}

class Family {
public:
    Family() {}
    explicit Family(const vector<Set>& sets) {
        for (const auto& set: sets) {
            sets_.insert(set);
        }
    }
    explicit Family(const set<Set>& sets) : sets_(sets) {}

    const set<Set>& getSets() const {
        return sets_;
    }

    size_t size() const {
        return sets_.size();
    }
    bool contains(const Set& set) {
        return sets_.contains(set);
    }

    Family unionMerge(const Family& family) const {
        set<Set> result = {};
        for (const auto& set_1: sets_) {
            for (const auto& set_2: family.sets_) {
                result.insert(set_1 | set_2);
            }
        }
        return Family(result);
    }
    void insert(const Set& set) {
        sets_.insert(set);
    }

protected:
    set<Set> sets_;
};
ostream& operator<<(ostream &os, const Family& family) {
    if (family.size() == 0) {
        os << "[]";
        return os;
    }

    os << "[";
    auto sets = family.getSets();
    auto iter = sets.begin();
    os << *iter;
    while(++iter != sets.end()) {
        os << ", " << *iter;
    }
    os << "]";
    return os;
}

Family hyperCube(int k) {
    Family hypercube;
    size_t hypercube_size = pow(2, k);
    for (size_t i = 0; i < hypercube_size; ++i) {
//        cout << i << endl;
        uint16_t number = 0;
        for (size_t j = 0; j < k; ++j) {
            number += (1 << j) * ((i & (1 << j)) != 0);
//            cout << i << " " << j << " " << (1 << j) << " " << number << endl;
        }
//        cout << number << endl;
        hypercube.insert(Set(number));
    }
    return hypercube;
}

struct NegSet {
    Set set;
    int weight;

    int operator<=>(const NegSet& other) const {
        if (other.weight != weight) {
            return weight - other.weight;
        }
        return set <=> other.set;
    }
};

Family getUnionClosedClosure(const Family& family) {
    if (family.size() == 1) {
        return family;
    }
    set<Set> sets = family.getSets();
    auto start_iter = sets.begin();
    while (true) {
        auto end_iter = next(start_iter);
        if (end_iter == sets.end()) {
            break;
        }

        while (end_iter != sets.end()) {
            sets.insert(*start_iter | *end_iter);
            ++end_iter;
        }
        ++start_iter;
    }
    return Family(sets);
}


class FCFamily : public Family {
public:
    FCFamily(const vector<Set>& sets, const vector<int>& weights, int r) : Family(sets), weights_(weights), r_(r) {
        this->insert(Set({}));
        closer_family = getUnionClosedClosure(*this);
        calculateT();
    }
    FCFamily(const set<Set>& sets, const vector<int>& weights, int r) : Family(sets), weights_(weights), r_(r) {
        closer_family = getUnionClosedClosure(*this);
        calculateT();
    }

    int calculateWeight(const Set& set) const {
      vector<int> numbers = set.getNumbers();
      int weight = 0;
      for (auto num: numbers) {
          weight += weights_[num - 1];
      }
      return weight - t_;
    }

    int calculateWeight(const Family& family) const {
        int result = 0;
        for (const Set& set: family.getSets()) {
            result += calculateWeight(set);
        }
        return result;
    }

    vector<NegSet> calculateNegativeSets(int k) const {
        if (k == 0) {
            return {{Set(0), -t_}};
        }

        Family hypercube = hyperCube(r_);

        vector<int> k_numbers;
        for (int i = r_ + 1; i < r_ + k + 1; ++i) {
            k_numbers.push_back(i);
        }
//        cout << "calculate neg sets 1" << endl;
        Set k_set = Set(k_numbers);
//        cout << "calculate neg sets 2" << endl;
        vector<Set> k_vector(1, k_set);
//        k_vector.emplace_back(0);
        Family k_family = Family(k_vector);
//        cout << "calculate neg sets 3" << endl;

        Family merged_families = hypercube.unionMerge(k_family);
//        cout << "calculate neg sets 4" << endl;

        vector<NegSet> negative_sets;
        for (const auto& set: merged_families.getSets()) {
            int weight = calculateWeight(set);
            if (weight < 0) {
                negative_sets.push_back({set, weight});
            }
        }
//        cout << "calculate neg sets 5" << endl;
//        negative_sets.push_back({Set(0), -t_});
        std::sort(negative_sets.begin(), negative_sets.end());
        return negative_sets;
    }

    Family closer_family;
private:
    void calculateT() {
        int sum = 0;
        for (int num: weights_) {
            sum += num;
        }
        t_ = sum / 2;
    }

    vector<int> weights_;
    int t_;
    int r_;
};

FCFamily readFCFamily(fstream& file) {
    int family_size, r;
    file >> family_size >> r;
//    cout << family_size << " " << r << endl;
    vector<int> weights(n, 0);
    int weight;
    for (size_t i = 0; i < r + 1; ++i) {
        file >> weight;
        weights[i] = weight;
    }
    for (size_t i = r + 1; i < n; ++i) {
        weights[i] = weight;
    }

    vector<Set> sets;
    for (size_t i = 0; i < family_size; ++i) {
        int set_len;
        file >> set_len;
        vector<int> numbers;
        for (size_t j = 0; j < set_len; ++j) {
            int cur_num;
            file >> cur_num;
            numbers.push_back(cur_num);
        }
        sets.emplace_back(numbers);
    }
    return FCFamily(sets, weights, r);
}

bool isEquivalent(const vector<vector<const Set*>>& family_1, const vector<vector<const Set*>>& family_2) {
    for (size_t i = 0; i < n; ++i) {
        if (family_1[i].size() > 0) {
            for (size_t j = i + 1; j < n + 1; ++j) {
                if (family_1[j].size() > 0) {
                    for (size_t k = 0; k < family_1[i].size(); ++k) {
                        for (size_t l = 0; l < family_1[j].size(); ++l) {
                            if ((*family_1[i][k] | *family_1[i][l]).size() != (*family_2[i][k] | *family_2[i][l]).size()) {
                                return false;
                            }
                        }
                    }
                }
            }
        }
    }
    return true;
}

vector<vector<const Set*>> getSubSetFromVector(vector<vector<size_t>> positions, const vector<vector<const Set*>>& sizes_families) {
    vector<vector<const Set*>> current_family(positions.size());
    for (size_t i = 0; i < positions.size(); ++i) {
        vector<const Set*> current_sets;
        for (size_t j = 0; j < positions[i].size(); ++j) {
            if (positions[i][j] == 1) {
                current_sets.push_back(sizes_families[i][j]);
            }
        }
        current_family[i] = current_sets;
    }
    return current_family;
}

bool checkEquivalent(vector<vector<const Set*>>& current_subfamily, vector<vector<const Set*>>& family, vector<vector<const Set*>>& fc_family, size_t i) {
    if (i == n + 1) {
        return isEquivalent(current_subfamily, fc_family);
    }
    current_subfamily[i].clear();
    for (size_t j = 0; j < fc_family[i].size(); ++j) {
        current_subfamily[i].push_back(family[i][j]);
    }
    bool answer = false;
    answer |= checkEquivalent(current_subfamily, family, fc_family, i + 1);
    if (answer) {
        return true;
    }
    while (next_permutation(family[i].begin(), family[i].end())) {
        current_subfamily[i].clear();
        for (size_t j = 0; j < fc_family[i].size(); ++j) {
            current_subfamily[i].push_back(family[i][j]);
        }
        answer |= checkEquivalent(current_subfamily, family, fc_family, i + 1);
        if (answer) {
            return true;
        }
    }
    return false;
}

void matchSets(size_t group_id, size_t pos, size_t cnt, const vector<size_t>& group_cnt, const vector<size_t>& group_len, const vector<vector<const Set*>>& sizes_families, vector<vector<size_t>& positions) {
    if (group_id == group_cnt.size()) {
        make_func(positions);
        return;
    }

    if (cnt > group_cnt[group_id] || cnt + group_len[group_id] - pos < group_cnt[group_id]) {
        return;
    }

    if (pos == group_len[group_id]) {
        if (cnt != group_cnt[group_id]) {
            return;
        }
        matchSets(group_id + 1, 0, 0, group_cnt, group_len, sizes_families, positions);
        return;
    }
    positions[group_id][pos] = 0;
    matchSets(group_id,  pos + 1, cnt, group_cnt, group_len, sizes_families, positions);
    positions[group_id][pos] = 1;
    matchSets(group_id,  pos + 1, cnt + 1, group_cnt, group_len, sizes_families, positions);
}

bool hasBaseFC(const Family& family) {
    vector<decltype(family.getSets().begin())> three_element_sets;
    for (auto iter = family.getSets().begin(); iter != family.getSets().end(); ++iter) {
        if (iter->size() > 0 && iter->size() <= 2) {
            return true;
        }
        if (iter->size() == 3) {
            three_element_sets.push_back(iter);
        }
    }
//    cout << "Base FC checking" << endl;
//    for (const auto& iter: three_element_sets) {
//        cout << *iter << " ";
//    }
//    cout << endl;
    for (size_t i = 0; i + 2 < three_element_sets.size(); ++i) {
        for (size_t j = i + 1; j + 1 < three_element_sets.size(); ++j) {
            for (size_t k = j + 1; k < three_element_sets.size(); ++k) {
                Set union_set = *three_element_sets[i];
                union_set = union_set | *three_element_sets[j];
                union_set = union_set | *three_element_sets[k];
                if (union_set.size() == 5) {
                    return true;
                }
                for (size_t l = k + 1; l < three_element_sets.size(); ++l) {
                    union_set = union_set | *three_element_sets[l];
                    if (union_set.size() >= 6 && union_set.size() <= 7) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}
bool hasFC(const Family& family, const FCFamily& fc_family) {
    vector<vector<const Set*>> sizes_set_1(n + 1);
    vector<vector<const Set*>> sizes_set_2(n + 1);

    for (const auto& set: family.getSets()) {
        sizes_set_1[set.size()].push_back(&set);
    }
    for (const auto& set: fc_family.getSets()) {
        sizes_set_2[set.size()].push_back(&set);
    }

    vector<vector<const Set*>> current_subfamily(n + 1);
    for (size_t i = 0; i < n + 1; ++i) {
        sort(sizes_set_2[i].begin(), sizes_set_2[i].end());
        sort(sizes_set_1[i].begin(), sizes_set_1[i].end());
        if (sizes_set_2[i].size() > sizes_set_1[i].size()) {
            return false;
        }
        for (size_t j = 0; i < sizes_set_2[i].size(); ++j) {
            current_subfamily[i].push_back(sizes_set_1[i][j]);
        }
    }


    return checkEquivalent(current_subfamily, sizes_set_1, sizes_set_2, 0);
}
bool hasPreviousFC(const Family& family) {
    return false;
}


Family getFamilyFromVector(const vector<int>& indexes, const vector<NegSet>& sets) {
    if (indexes.size() > sets.size()) {
        cout << "Index out of range" << endl;
        return {};
    }

    vector<Set> result;
    for (size_t i = 0; i < indexes.size(); ++i) {
        if (indexes[i]) {
            result.push_back(sets[i].set);
        }
    }
    if (result.size() == 0) {
        result.emplace_back(0);
    }
    return Family(result);
}

vector<int> d_weight = {1000, 1000};
void backtracking(const FCFamily& fc_family, const vector<NegSet>& neg_family, const Set& K, vector<int>& b) {
//    if (b.size() >= 2 && b[0] == 0 && b[1] == 1) {
//        cout << "b.size: " << b.size() << ", b:";
//        for (const auto &number: b) {
//            cout << number << " ";
//        }
//        cout << endl;
//    }
    Family G = fc_family.closer_family.unionMerge(getUnionClosedClosure(getFamilyFromVector(b, neg_family)));
//    cout << "G: " << G << endl;
//    cout << "hasBaseFC: " << hasBaseFC(G) << endl;
//    cout << "hasPreviousFC: " << hasPreviousFC(G) << endl;
    if (!hasBaseFC(G) && !hasPreviousFC(G)) {
        int g_weight = fc_family.calculateWeight(G);
        if (g_weight == 34) {
            cout << "G size, G: " << G.size() << " " << G << endl;
        }
        int u = g_weight;
        for (size_t i = b.size(); i < neg_family.size(); ++i) {
            if (!G.contains(neg_family[i].set)) {
                u += neg_family[i].weight;
            }
        }
        size_t index = static_cast<size_t>(G.contains(K));
//        cout << "g_weight: " << g_weight << " u: " << u << " index: " << index << endl;
        if (u < d_weight[index]) {
            if (g_weight < d_weight[index]) {
                cout << "fc_family.closer_family: " << fc_family.closer_family << endl;
                cout << "getFamilyFromVector(b, neg_family)): " << getFamilyFromVector(b, neg_family) << endl;
                cout << "getUnionClosedClosure(getFamilyFromVector(b, neg_family)): " << getUnionClosedClosure(getFamilyFromVector(b, neg_family)) << endl;
                cout << "Index: " << index << " g_weight: " << g_weight << " G: " << G << endl;
                cout << endl;
            }
            d_weight[index] = min(g_weight, d_weight[index]);
        }
        if (b.size() < neg_family.size()) {
            b.push_back(1);
            backtracking(fc_family, neg_family, K, b);
            b.pop_back();
            if (!G.contains(neg_family[b.size() + 1].set)) {
                b.push_back(0);
                backtracking(fc_family, neg_family, K, b);
                b.pop_back();
            }
        }
    }
}

int main() {
    fstream file(INPUT_FILE, ios_base::in);
    int sets_len = 0;
    int total_families = 0;
    file >> sets_len >> total_families;
    n = sets_len;
    FCFamily first_family = readFCFamily(file);
    cout << "FC family: " << first_family << endl;
    vector<NegSet> negative_sets = first_family.calculateNegativeSets(5);
    cout << "negative_sets.size: " << negative_sets.size() << endl;
    vector<int> K = {6, 7, 8, 9, 10};
    vector<int> b = {1};

    cout << "Negative sets: ";
    for (const auto& neg_set: negative_sets) {
        cout << neg_set.set << ": " << neg_set.weight << ", ";
    }
    cout << endl;

    cout << "K: " << K << endl;

    backtracking(first_family, negative_sets, Set(K), b);

    b = {0};
//    vector<int> b = {0};
    backtracking(first_family, negative_sets, Set(K), b);

//    cout << hasBaseFC()
    cout << first_family << endl;

    cout << "d_weight: " << d_weight[0] << " " << d_weight[1] << endl;
}