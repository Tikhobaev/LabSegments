#include <algorithm>
#include <vector>
#include <set>
#include <iterator>
#include <iostream>
#include <utility>
#include <chrono>
#include "AvlTree.h"
#include "TwoThreeTree.h"

# define not_found -1
#define PI 3.14159265

using std::swap;
using std::min;
using std::max;
using std::vector;
using std::set;
using std::iterator;
using std::pair;
using std::make_pair;
using std::cout;
using std::cin;
using std::endl;

const double EPS = 1E-9;

struct pt {
    double x, y;
};

struct seg {
    pt p, q;
    int id;

    double get_y(double x) const {
        if (abs(p.x - q.x) < EPS)  return p.y;
        return p.y + (q.y - p.y) * (x - p.x) / (q.x - p.x);
    }

    bool operator <(const seg& rhs) {
        return q.y < rhs.q.y;
    }

    bool operator >(const seg& rhs) {
        return q.y > rhs.q.y;
    }

    bool operator ==(const seg& rhs) {
        return q.y == rhs.q.y;
    }

    bool operator !=(const seg& rhs) {
        return q.y != rhs.q.y;
    }
};


inline bool intersect1d(double l1, double r1, double l2, double r2) {
    if (l1 > r1)  swap(l1, r1);
    if (l2 > r2)  swap(l2, r2);
    return max(l1, l2) <= min(r1, r2) + EPS;
}

inline int vec(const pt & a, const pt & b, const pt & c) {
    double s = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    return abs(s) < EPS ? 0 : s > 0 ? +1 : -1;
}

bool intersect(const seg & a, const seg & b) {
    return intersect1d(a.p.x, a.q.x, b.p.x, b.q.x)
        && intersect1d(a.p.y, a.q.y, b.p.y, b.q.y)
        && vec(a.p, a.q, b.p) * vec(a.p, a.q, b.q) <= 0
        && vec(b.p, b.q, a.p) * vec(b.p, b.q, a.q) <= 0;
}


bool operator< (const seg & a, const seg & b) {
    double x = max(min(a.p.x, a.q.x), min(b.p.x, b.q.x));
    return a.get_y(x) < b.get_y(x) - EPS;
}


struct event {
    double x;
    int tp, id;

    event() { }
    event(double x, int tp, int id)
        : x(x), tp(tp), id(id)
    { }

    bool operator< (const event & e) const {
        if (abs(x - e.x) > EPS)  return x < e.x;
        return tp > e.tp;
    }
};


inline seg* prev(seg& x, AVL<seg> avl) {
    auto found = avl.prev(x);
    if (found == NULL) {
        return NULL;
    }
    
    return &(found->key);
}

inline seg* next(seg& x, AVL<seg> avl) {
    auto found = avl.next(x);
    if (found == NULL) {
        return NULL;
    }

    return &(found->key);
}

pair<int, int> solve(vector<seg> & a) {
    AVL<seg> avl;
    int n = (int)a.size();
    vector<event> e;
    for (int i = 0; i < n; ++i) {
        e.push_back(event(min(a[i].p.x, a[i].q.x), +1, i));
        e.push_back(event(max(a[i].p.x, a[i].q.x), -1, i));
    }
    sort(e.begin(), e.end());

    for (size_t i = 0; i < e.size(); ++i) {
        int id = e[i].id;
        auto current_event = e[i];
        if (e[i].tp == +1) {
            avl.insert(a[id]);
            auto vsize = a.size();
            if (id == 2849 && vsize == 5000) {
                auto mmm = 1;
            }
            auto nxt = next(a[id], avl);
            auto prv = prev(a[id], avl);
            if (nxt != NULL && intersect(*nxt, a[id]))
                return make_pair(nxt->id, id);
            if (prv != NULL && intersect(*prv, a[id]))
                return make_pair(prv->id, id);
        }
        else {  
            auto nxt = next(a[id], avl);
            auto prv = prev(a[id], avl);
            if (nxt != NULL && prv !=NULL && intersect(*nxt, *prv))
                return make_pair(prv->id, nxt->id);
            avl.remove(a[id]);
        }
    }

    return make_pair(-1, -1);
}


inline seg* prev23(seg& x, two3Tree<seg>& tree) {
    return tree.prev(x);
}

inline seg* next23(seg& x, two3Tree<seg>& tree) {
    return tree.next(x);
}

pair<int, int> solve23tree(vector<seg> & a) {
    two3Tree<seg> tree;
    int n = (int)a.size();
    vector<event> e;
    for (int i = 0; i < n; ++i) {
        e.push_back(event(min(a[i].p.x, a[i].q.x), +1, i));
        e.push_back(event(max(a[i].p.x, a[i].q.x), -1, i));
    }
    sort(e.begin(), e.end());

    for (size_t i = 0; i < e.size(); ++i) {
        int id = e[i].id;
        auto current_event = e[i];
        if (e[i].tp == +1) {
            tree.insert(a[id]);
            auto nxt = next23(a[id], tree);
            auto prv = prev23(a[id], tree);
            if (nxt != NULL && intersect(*nxt, a[id]))
                return make_pair(nxt->id, id);
            if (prv != NULL && intersect(*prv, a[id]))
                return make_pair(prv->id, id);
        }
        else {
            auto nxt = next23(a[id], tree);
            auto prv = prev23(a[id], tree);
            if (prv != NULL && nxt != NULL && intersect(*nxt, *prv))
                return make_pair(prv->id, nxt->id);
            tree.deleteNode(a[id]);
        }
    }

    return make_pair(-1, -1);
}


void test23() {
    two3Tree<int> tree1;
    
    int userInput = 0;
    int data;
    
    while (userInput != -1)
    {
        tree1.displayFunctions();
        cin >> userInput;
    
        if (userInput == 1)
        {
            cout << "\nEnter the data to be inserted into the Tree : ";
            cin >> data;
    
            tree1.insert(data);
    
            cout << "\nAfter insertion :";
            tree1.printTree();
        }
    
        else if (userInput == 2)
        {
            cout << "\nEnter the data to be deleted from the Tree : ";
            cin >> data;
    
            tree1.deleteNode(data);
    
            cout << "\nAfter deletion :";
            tree1.printTree();
        }
    
        else if (userInput == 3)
        {
            cout << "\nEnter the data element which is to be searched : ";
            cin >> data;
    
            if (tree1.searchFor(data) != NULL)
                cout << "\nThe element is found. ";
            else
                cout << "\nThe element is not found. ";
        }
    
        else if (userInput == 4)
        {
            tree1.printTree();
        }
    }
}

vector<seg>* generateData(int size) {
    vector<seg>* output = new vector<seg>;
    for (int i = 0; i < size; i++) {
        double startx = i % 2 == 0 ? double(i) : double(i);
        double starty = i % 2 == 0 ? double(i) : double(i + 1);
        double endx = i % 2 == 0 ? double(i + 1) : double(i + 1);
        double endy = i % 2 == 0 ? double(i + 1) : double(i);
        output->push_back(seg{ pt{startx, starty}, pt{endx, endy}, i });
    }
    return output;
}

// Generate segments with start and end points in square angles with a = 1
vector<seg>* generateDataFirstApproach(int size) {
    vector<seg>* output = new vector<seg>;
    for (int i = 0; i < size; i++) {
        double start_x, start_y, end_x, end_y;
        int squareBottomLeftPoint = rand() % 1000;
        int startPoint = rand() % 4; // 1 - bottom left, 2 - br, 3 - upper left, 4 - ur
        vector<pair<double, double>> squarePoints{
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint),          // BL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint),      // BR
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint + 1),      // UL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint + 1)   // UR
        };
        start_x = squarePoints[startPoint].first;
        start_y = squarePoints[startPoint].second;
        squarePoints.erase(squarePoints.begin() + startPoint);
        
        int endPoint = rand() % 3; // remaining 3 points
        end_x = squarePoints[endPoint].first;
        end_y = squarePoints[endPoint].second;
        // cout << "{ (" << start_x << ", " << start_y << "), (" << end_x << ", " << end_y << ") }";
        output->push_back(seg{ pt{start_x, start_y}, pt{end_x, end_y}, i });
    }
    return output;
}

// Generate segments with start and end points in square angles with a = 1
vector<seg>* generateDataFirstApproachNNotIntersected(int size, int numNotIntersected) {
    vector<seg>* output = new vector<seg>;
    for (int i = 0; i < numNotIntersected; i++) {
        double start_x, start_y, end_x, end_y;
        int squareBottomLeftPoint = i * 2;
        int startPoint = rand() % 4; // 1 - bottom left, 2 - br, 3 - upper left, 4 - ur
        vector<pair<double, double>> squarePoints{
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint),          // BL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint),      // BR
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint + 1),      // UL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint + 1)   // UR
        };
        start_x = squarePoints[startPoint].first;
        start_y = squarePoints[startPoint].second;
        squarePoints.erase(squarePoints.begin() + startPoint);

        int endPoint = rand() % 3; // remaining 3 points
        end_x = squarePoints[endPoint].first;
        end_y = squarePoints[endPoint].second;
        // cout << "{ (" << start_x << ", " << start_y << "), (" << end_x << ", " << end_y << ") }";
        output->push_back(seg{ pt{start_x, start_y}, pt{end_x, end_y}, i });
    }

    int offset = numNotIntersected * 2;

    for (int i = numNotIntersected; i < size; i++) {
        double start_x, start_y, end_x, end_y;
        int squareBottomLeftPoint = offset + rand() % 1000;
        int startPoint = rand() % 4; // 1 - bottom left, 2 - br, 3 - upper left, 4 - ur
        vector<pair<double, double>> squarePoints{
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint),          // BL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint),      // BR
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint + 1),      // UL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint + 1)   // UR
        };
        start_x = squarePoints[startPoint].first;
        start_y = squarePoints[startPoint].second;
        squarePoints.erase(squarePoints.begin() + startPoint);

        int endPoint = rand() % 3; // remaining 3 points
        end_x = squarePoints[endPoint].first;
        end_y = squarePoints[endPoint].second;
        // cout << "{ (" << start_x << ", " << start_y << "), (" << end_x << ", " << end_y << ") }";
        output->push_back(seg{ pt{start_x, start_y}, pt{end_x, end_y}, i });
    }
    return output;
}

// Generate segments with start and end points in square angles with a = 1
vector<seg>* generateDataSecondApproach(int size, int len) {
    vector<seg>* output = new vector<seg>;
    for (int i = 0; i < size; i++) {
        double start_x, start_y, end_x, end_y;
        int squareBottomLeftPoint = rand() % 1000;
        int startPoint = rand() % 4; // 1 - bottom left, 2 - br, 3 - upper left, 4 - ur
        vector<pair<double, double>> squarePoints{
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint),          // BL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint),      // BR
            make_pair(squareBottomLeftPoint, squareBottomLeftPoint + 1),      // UL
            make_pair(squareBottomLeftPoint + 1, squareBottomLeftPoint + 1)   // UR
        };
        start_x = squarePoints[startPoint].first;
        start_y = squarePoints[startPoint].second;
        
        int angle = (rand() % 90) * PI / 180;
        end_x = cos(angle) * len + start_x;
        end_y = sin(angle) * len + start_y;
        output->push_back(seg{ pt{start_x, start_y}, pt{end_x, end_y}, i });
    }
    return output;
}

void test() {
    for (int i = 1000; i <= 10000; i += 1000) {
        cout << "N = " << i << endl;
        auto data = generateData(i);
        std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
        auto result = solve(*data);
        std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
        if (result.first == -1 && result.second == -1) {
            cout << "No intersecting segments" << endl;
        }
        else {
            cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
        }
        std::cout << "Time = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << " ms" << std::endl;
    }
}

void testFirstApproachAvl() {
    for (int i = 1000; i <= 10000; i += 1000) {
        cout << "N = " << i << endl;
        auto data = generateDataFirstApproach(i);
        std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
        auto result = solve(*data);
        std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
        if (result.first == -1 && result.second == -1) {
            cout << "No intersecting segments" << endl;
        }
        else {
            cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
            cout << "{ (" << (*data)[result.first].p.x << ", " << (*data)[result.first].p.y << "), (" << (*data)[result.first].q.x << ", " << (*data)[result.first].q.y << ") }, ";
            cout << "{ (" << (*data)[result.second].p.x << ", " << (*data)[result.second].p.y << "), (" << (*data)[result.second].q.x << ", " << (*data)[result.second].q.y << ") }" << endl;
        }
        std::cout << "Time = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << " ms" << std::endl;
    }
}

void testFirstApproachNNotIntersectedAvl() {
    for (int i = 1000; i <= 10000; i += 1000) {
        cout << "N = " << i << endl;
        auto data = generateDataFirstApproachNNotIntersected(i, i - 100);
        std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
        auto result = solve(*data);
        std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
        if (result.first == -1 && result.second == -1) {
            cout << "No intersecting segments" << endl;
        }
        else {
            cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
            cout << "{ (" << (*data)[result.first].p.x << ", " << (*data)[result.first].p.y << "), (" << (*data)[result.first].q.x << ", " << (*data)[result.first].q.y << ") }, ";
            cout << "{ (" << (*data)[result.second].p.x << ", " << (*data)[result.second].p.y << "), (" << (*data)[result.second].q.x << ", " << (*data)[result.second].q.y << ") }" << endl;
        }
        std::cout << "Time = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << " ms" << std::endl;
    }
}

void testFirstApproach23() {
    for (int i = 1000; i <= 10000; i += 1000) {
        cout << "N = " << i << endl;
        auto data = generateDataFirstApproach(i);
        std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
        auto result = solve23tree(*data);
        std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
        if (result.first == -1 && result.second == -1) {
            cout << "No intersecting segments" << endl;
        }
        else {
            cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
            cout << "{ (" << (*data)[result.first].p.x << ", " << (*data)[result.first].p.y << "), (" << (*data)[result.first].q.x << ", " << (*data)[result.first].q.y << ") }, ";
            cout << "{ (" << (*data)[result.second].p.x << ", " << (*data)[result.second].p.y << "), (" << (*data)[result.second].q.x << ", " << (*data)[result.second].q.y << ") }" << endl;
        }
        std::cout << "Time = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << " ms" << std::endl;
    }
}

void testSecondApproachAvl() {
    for (int i = 1000; i <= 10000; i += 1000) {
        cout << "N = " << i << endl;
        auto data = generateDataSecondApproach(i, 10);
        std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
        auto result = solve(*data);
        std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
        if (result.first == -1 && result.second == -1) {
            cout << "No intersecting segments" << endl;
        }
        else {
            cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
            cout << "{ (" << (*data)[result.first].p.x << ", " << (*data)[result.first].p.y << "), (" << (*data)[result.first].q.x << ", " << (*data)[result.first].q.y << ") }, ";
            cout << "{ (" << (*data)[result.second].p.x << ", " << (*data)[result.second].p.y << "), (" << (*data)[result.second].q.x << ", " << (*data)[result.second].q.y << ") }" << endl;
        }
        std::cout << "Time = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << " ms" << std::endl;
    }
}

void testSecondApproach23() {
    for (int i = 1000; i <= 10000; i += 1000) {
        cout << "N = " << i << endl;
        auto data = generateDataSecondApproach(i, 10);
        std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
        auto result = solve23tree(*data);
        std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
        if (result.first == -1 && result.second == -1) {
            cout << "No intersecting segments" << endl;
        }
        else {
            cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
            cout << "{ (" << (*data)[result.first].p.x << ", " << (*data)[result.first].p.y << "), (" << (*data)[result.first].q.x << ", " << (*data)[result.first].q.y << ") }, ";
            cout << "{ (" << (*data)[result.second].p.x << ", " << (*data)[result.second].p.y << "), (" << (*data)[result.second].q.x << ", " << (*data)[result.second].q.y << ") }" << endl;
        }
        std::cout << "Time = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << " ms" << std::endl;
    }
}

int main() {
    // test23();

    vector<seg> input = {
        seg{pt {2, 2}, pt {5, 5}, 0},
        seg{pt {6, 5}, pt {7, 7}, 1},
        seg{pt {6, 7}, pt {7, 5}, 2},
        seg{pt {7, 4}, pt {9, 1}, 3},
        seg{pt {8, 1}, pt {11, 3}, 4},
        seg{pt {11, 4}, pt {13, 2}, 5}
    };
    // auto result = solve(input);
    /*auto result = solve23tree(input);
    if (result.first == -1 && result.second == -1) {
        cout << "No intersecting segments" << endl;
    }
    else {
        cout << "Found intersecting lines, ids: " << result.first << ", " << result.second << endl;
    }*/
    // test();
    // testFirstApproachAvl();
    // testFirstApproach23();
    // testSecondApproach23();
    // testSecondApproachAvl();
    testFirstApproachNNotIntersectedAvl();
}