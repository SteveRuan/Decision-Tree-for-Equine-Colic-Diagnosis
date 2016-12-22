#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <cstdlib>
#include <vector>
#include <math.h>
#include <algorithm>

using namespace std;



// Let healthy = 0, colic = 1.

// examples are vector of vector of double
// vector < vector <double> > examplelist;

// attributes is an integer storing binary data that whether 2^i is checked


// a structure for return the best attribute with a value
struct best_attribute {
  int attribute;
  double value;
};

// a structure for a node of the tree
struct node {
  int attribute;   // the attribute is chosen
  double value;   // the value of attribute
  int classification;   // healthy = 0, colic = 1, not leaves = -1.
  struct node* Usubtree;   // two children
  struct node* Lsubtree;
};


// read the input file horseTrain.txt and horseTest.txt to get a example list
vector < vector <double> > read_examples (string filename) {
  vector < vector <double> > ex;
  string read_str, next_data;
  int comma_posn;

  ifstream fin;
  fin.open(filename.c_str());   // open the file
    
  // use a while loop to read lines
  while (1) {
    getline(fin, read_str);
    if (fin.eof()) break;   // if EOF, jump out the loop
    vector <double> data;   // store data in vector
      
    // read data that seperated by comma
    for (int j = 0; j < 16; j += 1) {
      comma_posn = read_str.find(',');
      next_data = read_str.substr(0,comma_posn-1);
      data.push_back(atof(next_data.c_str()));
      read_str = read_str.substr(comma_posn+1);
    }

    if (read_str.size() >= 7) data.push_back(0);
    else data.push_back(1);

    ex.push_back(data);   // push the example in example list
  }
  fin.clear();
  fin.close();
  return ex;
}


// determine whether all examples have the same classification
bool same_classifications (vector < vector <double> > ex) {
  if (ex.size() == 0) return true;
  else {
    double cl = ex.at(0).back();
    for (int i = 0; i < ex.size(); i += 1) {
      if (ex.at(i).back() != cl) return false;   // if not the same, return false
    }
    return true;
  }
}


// find the mode of classifications
int example_mode (vector < vector <double> > ex) {
  int ones = 0, zeros = 0;   // count of 0's and 1's
  for (int i = 0; i < ex.size(); i += 1) {
    if (ex.at(i).back() == 1) ones += 1;
    else if (ex.at(i).back() == 0) zeros += 1;
  }
  if (ones > zeros) return 1;   // if more 1's than 0's return 1
  else return 0;   // otherwise 0
}


// divide the example list ino two part by the attribute
vector < vector <double> > LU_examples (vector < vector <double> > ex, best_attribute best, int LorU) {
  vector < vector <double> > subex;
  for (int i = 0; i < ex.size(); i += 1) {
    if ((!LorU && ex.at(i).at(best.attribute) <= best.value) || (LorU && ex.at(i).at(best.attribute) > best.value)) {
      subex.push_back(ex.at(i));   // when LorU = 0, return the lower list, otherwise return the upper list
    }
  }
  return subex;
}

// calculte the information gain
double IG (int attribute, double value, vector < vector <double> > ex) {
  double LH = 0, LC = 0, UH = 0, UC = 0, PH, PC, PLH, PLC, PUH, PUC, IHC, ILHC, IUHC;

  if (ex.size() == 0) return 0;
  else {
    // count the number of Lower&Healthy, Lower&Colic, Upper&Healthy and Upper&Colic in the whole example list 
    for (int i = 0; i < ex.size(); i += 1) {
      if (ex.at(i).at(attribute) <= value) {
        if (ex.at(i).back()) LC += 1;
        else LH += 1;
      } else {
        if (ex.at(i).back()) UC += 1;
        else UH += 1;
      }
    }
            
    PH = (LH + UH) / (double)ex.size();   // probability of healthy
    PC = (LC + UC) / (double)ex.size();   // probability of colic
    // information content
    if (PH && PC) IHC = -(PH * log2(PH)) - (PC * log2(PC));
    else if (PH && !PC) IHC = -(PH * log2(PH));
    else if (!PH && PC) IHC = -(PC * log2(PC));
    else IHC = 0;

    if (LH + LC == 0) {
      PLH = 0;
      PLC = 0;
    } else {
      PLH = LH / (LH + LC);   // probability of lower&healthy
      PLC = LC / (LH + LC);   // probability of lower&colic
    }
    // information content
    if (PLH && PLC) ILHC = -(PLH * log2(PLH)) - (PLC * log2(PLC));
    else if (PLH && !PLC) ILHC = -(PLH * log2(PLH));
    else if (!PLH && PLC) ILHC = -(PLC * log2(PLC));
    else ILHC = 0;
      
    if (UH + UC == 0) {
      PUH = 0;
      PUC = 0;
    } else {
      PUH = UH / (UH + UC);   // probability of upper&healthy
      PUC = UC / (UH + UC);   // probability of upper&colic
    }
    // information content
    if (PUH && PUC) IUHC = -(PUH * log2(PUH)) - (PUC * log2(PUC));
    else if (PUH && !PUC) IUHC = -(PUH * log2(PUH));
    else if (!PUH && PUC) IUHC = -(PUC * log2(PUC));
    else ILHC = 0;
      
    // Information Gain (IG)
    double result = IHC - ((LH + LC) / (double)ex.size() * ILHC) - ((UH + UC) / (double)ex.size() * IUHC);
      
    return result;
  }
}


// choose the attribute with value
best_attribute choose_attribute (int at, vector < vector <double> > ex) {
  best_attribute best;
  double maxIG = -1, curIG, attribute_value;   // maximum IG, current IG and the value of attribute
  vector < vector <double> > attributelist;

  // make a list by attribute
  for (int i = 0; i < 16; i += 1) {
    vector <double> data;
    for (int j = 0; j < ex.size(); j += 1) {
      data.push_back(ex.at(j).at(i));
    }
    if (at & (int)(pow(2,i))) {   // if the attribute is not chosen yet
      sort(data.begin(),data.end());   // sort the value to find suitable value
    }
    attributelist.push_back(data);
  }

  for (int i = 0; i < 16; i += 1) {
    if (at & (int)(pow(2,i))) {   // if the attribute is not chosen yet
      for (int j = 1; j < attributelist.at(i).size(); j += 1) {

        if (attributelist.at(i).at(j-1) != attributelist.at(i).at(j)) {
          attribute_value = attributelist.at(i).at(j-1) / 2 + attributelist.at(i).at(j) / 2;   // try all mean to be the value of the attribute
              
          curIG = IG(i, attribute_value, ex);
              
          if (curIG > maxIG) {   // if the current IG > maximum IG, update it
            maxIG = curIG;
            best.attribute = i;
            best.value = attribute_value;
          }
        }
      }
    }
  }
  return best;
}

// the function that build a tree
node* DTL (vector < vector <double> > ex, int at, int classification) {
  node* treenode = new node;

  if (ex.size() == 0) treenode->classification = classification;   // if no example, return default value
  else if (same_classifications(ex)) treenode->classification = example_mode(ex);   // if all examples have the same classfications, return the mode classfication which is that classfication
  else if (at == 0) treenode->classification = example_mode(ex); // if all attributes have be chosen, return the mode classfication
  else {
    best_attribute best = choose_attribute(at,ex);   // find the best attribute
      
    // cut into two parts
    vector < vector <double> > Uex, Lex;
    Lex = LU_examples(ex,best,0);
    Uex = LU_examples(ex,best,1);
      
    // make the node
    treenode->attribute = best.attribute;
    treenode->value = best.value;
    treenode->classification = -1;
      
    // build the children
    treenode->Lsubtree = DTL(Lex,(at-pow(2,best.attribute)),example_mode(Lex));
    treenode->Usubtree = DTL(Uex,(at-pow(2,best.attribute)),example_mode(Uex));
  }

  return treenode;
}


// print out the tree
void printtree (node* node, int level) {
  for (int i = 0; i < level; i += 1) {cout << " ";}
  cout << level << ")." << "   ";
  if (node->classification == 0) cout << "healthy" << endl;   // leaves cases 
  else if (node->classification == 1) cout << "colic" << endl;   // leaves cases
  else {
    cout << node->attribute << "   " << node->value << endl;
      
    // print its children
    printtree(node->Lsubtree,level+1);
    printtree(node->Usubtree,level+1);
  }
}


// find the prediction by the tree
int runtree (vector <double> data, node* node) {
  if (node->classification >= 0) return node->classification;   // leaves cases
  else if (data.at(node->attribute) <= node->value) return runtree(data,node->Lsubtree);   // go lower child
  else return runtree(data,node->Usubtree);   // go upper child
}


// predict all examples and output the result
void runtree_result (vector < vector <double> > ex, node* rootnode) {
  int result, correctnum = 0, incorrectnum = 0;   // count of correct results and incorrect results
  
  for (int i = 0; i < ex.size(); i += 1) {
    result = runtree(ex.at(i),rootnode);   // run the predition
      
    
    // print the result
    cout << "Predict: ";
    if (result) cout << "Colic.";
    else cout << "Healthy.";
    cout << "   Actual: ";
    if (ex.at(i).back()) cout << "Colic." << endl;
    else cout << "Healthy." << endl;
      
    cout << "Test " << i << " is ";
    if (result == ex.at(i).back()) {   // compare the classifications
      cout << "correct." << endl;
      correctnum += 1;
    } else {
      cout << "incorrect." << endl;
      incorrectnum += 1;
    }
  }
  cout << correctnum << "/" << (correctnum + incorrectnum) << " of the examples is classify correctly." << endl;
}


// free the tree by recursion
void freetree (struct node* node) {
  if (node->classification >= 0) {
    free(node);
  } else {
    freetree(node->Lsubtree);
    freetree(node->Usubtree);
    free(node);
  }
}



int main () {
    
  // read input train data
  vector < vector <double> > trainex = read_examples("horseTrain.txt");

  // build a decision tree
  node* rootnode = DTL(trainex,(pow(2,16)-1),example_mode(trainex));
  
  //print out the tree
  cout << "The tree looks like:" << endl;
  printtree(rootnode,0);
  cout << endl;
    
  // read input test data
  vector < vector <double> > testex = read_examples("horseTest.txt");
    
  // run train examples and test examples on the tree
  cout << "Train examples:" << endl;
  runtree_result(trainex,rootnode);
  cout << endl << "Test examples:" << endl;
  runtree_result(testex,rootnode);

  // free the tree
  freetree(rootnode);
  return 0;
}
