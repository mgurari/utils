package mathAids is

	type row is array(integer range<>) of float;
	type column is array(integer range<>) of float;
	type matrix is array(integer range<>,integer range<>) of float;

	function diff(X1,Y1:row) return matrix;
	function integ(X2,Y2:row;takeAbs:boolean) return float;
	function formIdentMat(size:integer) return matrix;

end mathAids;
