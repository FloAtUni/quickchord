#include <iostream>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <ios>
using namespace std;

string gen_random(int length) {
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    string s;
    s.resize(length);
    for (int i = 0; i < length; ++i) {
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }
    return s;
}

int main(int argc, char *argv[]) {
  ios_base::sync_with_stdio(false);
  srand(time(NULL));
  if(argc < 2) {
    cout << "Usage: ./generate NumberOfLines\n";
  }
  int length = atoi(argv[1]);
  for(int i = 0; i < length; i++) {
    string key = gen_random(7);
    string value = gen_random(5);
    cout << "{\"" << key << "\", \"" << value << "\"}.\n";
  }
}
