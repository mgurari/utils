with Ada.Text_IO; use Ada.Text_IO;

package body mathAids is

	function diff(X1,Y1:dataArr) return resultArr is
		length:integer:=X1'length;
		result:resultArr(1..2,1..length);
		x:dataArr(1..length):=X1;
		y:dataArr(1..length):=Y1;
	begin
		for i in 1..(length-1) loop
			result(1,i):=(x(i+1)+x(i))/2.0;
			result(2,i):=(y(i+1)-y(i))/(x(i+1)-x(i));
		end loop;
		return result;	
	end diff;

	function integ(X2,Y2:dataArr;takeAbs:boolean) return float is
		length:integer:=X2'length;
		result:float:=0.0;
		x:dataArr(1..length):=X2;
		y:dataArr(1..length):=Y2;
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

end mathAids;
