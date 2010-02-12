package spaken.model;

import spaken.model.elements.*;
import spaken.model.pattern.Pattern;
import spaken.model.pattern.ExtractPattern;

public class Example {
	public static void main(String[] args) throws ImaginaryPointException {
		Construct sp = new Construct();
		Element p1 = sp.makePoint(null, new Pos(0, 0));
		Element p2 = sp.makePoint(null, new Pos(10,6));
		Element c1 = sp.circle(p1, p1, p2);
		Element c2 = sp.circle(p2, p1, p2);
		Points  is = (Points) sp.intersectCC(c1, c2);
		Element ll = sp.line(is.getPoint(0), is.getPoint(1));
		Element lo = sp.line(p1, p2);
		Point middel = (Point) sp.intersectLL(ll, lo);
		
		System.out.println(middel.getPos());
		
		// serialize and deserialize the whole bunch
		ExtractPattern ser = new ExtractPattern();
		middel.visit(ser);
		Pattern pat = ser.makePattern();
		Element re = pat.reconstruct(new Construct());
		
		System.out.println(middel);
		System.out.println(re);
	}
}
