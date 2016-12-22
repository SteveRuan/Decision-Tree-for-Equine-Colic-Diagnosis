# Decision-Tree-for-Equine-Colic-Diagnosis

A decision tree algorithm that will learn to diagnose whether a patient is healthy or has colic.

Use the horseTrain file, (found in the Assignment 4 folder) to train the decision
tree. Each training instance has 16 numeric attributes (features) and a classification, all separated
by commas. The attributes correspond to the following measurements made from each patient at
admission to the clinic.

1. K

2. Na

3. CL

4. HCO3

5. Endotoxin

6. Aniongap

7. PLA2

8. SDH

9. GLDH

10. TPP

11. Breath rate

12. PCV

13. Pulse rate

14. Fibrinogen

15. Dimer

16. FibPerDim

In the decision tree, use only binary tests, i.e. each node should test whether a particular
attribute has a value greater or smaller than a threshold. In deciding which attribute to test at
any point, use the information gain metric. Set the node test threshold for each potential attribute
using this same metric i.e. at each point, see all the values that exist for a particular attribute in
the remaining instances, order those values, and try threshold values that are (half way) between
successive attribute values. Use the threshold value that gives the highest information gain. Allow
the same attribute to be tested again later in the tree (with a different threshold). This means that
along a path from the root to a leaf, the same attribute might be tested multiple times. After learning
the decision tree, use the horseTest file to test the generalization accuracy of the tree.
