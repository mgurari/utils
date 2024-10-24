package body mathAids is

	function diff(X1,Y1:row) return matrix is
		length:integer:=X1'length;
		result:matrix(1..2,1..length);
		x:row(1..length):=X1;
		y:row(1..length):=Y1;
	begin
		for i in 1..(length-1) loop
			result(1,i):=(x(i+1)+x(i))/2.0;
			result(2,i):=(y(i+1)-y(i))/(x(i+1)-x(i));
		end loop;
		return result;	
	end diff;

	function integ(X2,Y2:row;takeAbs:boolean) return float is
		length:integer:=X2'length;
		result:float:=0.0;
		x:row(1..length):=X2;
		y:row(1..length):=Y2;
	begin
		case takeAbs is
			when true=>null;
			when false=>goto skip;
		end case;
		for i in 1..y'length loop
			if y(i)<0.0 then y(i):=-y(i); end if;
		end loop;
		<<skip>>
		for i in 1..(length-1) loop
			result:=result+(x(i+1)-x(i))*(y(i)+y(i+1))/2.0;
		end loop;
		return result;
	end integ;

	function formIdentMat(size:integer) return matrix is
		result:matrix(1..size,1..size);
	begin
		for i in 1..size loop
			for j in 1..size loop
				if i=j then 
					result(i,j):=(1.0);
				else
					result(i,j):=(0.0);
				end if;
			end loop;
		end loop;
		return result;
	end formIdentMat;

end mathAids;
