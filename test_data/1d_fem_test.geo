//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {1, 0, 0, 1.0};

//+
Point(3) = {1.5, 0.0, 0.0, 1.0};

//+
Line(1) = {1, 2};

//+
Line(2) = {2, 3};

//+
Physical Point("b_left") = {1};
//+
Physical Point("b_right", 2) = {3};
//+
Physical Curve("p_fuel", 3) = {1, 2};

