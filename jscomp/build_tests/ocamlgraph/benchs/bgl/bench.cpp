#include <string>                  
#include <iostream>
#include <time.h>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/random.hpp>
#include <boost/graph/depth_first_search.hpp>

#include <boost/random/linear_congruential.hpp>

using namespace boost;
using namespace std;

int cpt = 0;

class my_dfs_visitor:public default_dfs_visitor {
public:
  template < typename Vertex, typename Graph >
  void discover_vertex(const Vertex& u, const Graph& g) {
    cpt++;
    cout << cpt << "\t" << u << "\n";
  }
};

int main(int _, char* args[]) {
  // create a typedef for the Graph type
  typedef adjacency_list<vecS, vecS> Graph;

  minstd_rand ran(time(0));

  GraphvizDigraph g;
  read_graphviz(args[1], g);
  //  generate_random_graph(g, atoi(args[1]), atoi(args[2]), ran);
  //  write_graphviz(cout, g);
  default_dfs_visitor vis;
  // my_dfs_visitor vis;
  time_t t1, t2; 
  time(&t1);
  depth_first_search(g, visitor(vis));
  time(&t2);
  double dif = difftime(t2, t1);
  cout << dif << " seconds.\n";
  
  return 0;
}

/*
Local Variables:
compile-command: "g++ -O3 -o cpp_bench -Wall -lbgl-viz bench.cpp"
End:
*/
