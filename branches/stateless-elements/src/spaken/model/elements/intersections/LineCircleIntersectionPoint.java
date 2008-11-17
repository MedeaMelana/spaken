package spaken.model.elements.intersections;

import java.io.IOException;
import java.util.Collection;

import spaken.model.*;
import spaken.model.elements.*;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

class LineCircleIntersectionPoint extends AbstractPoint {
	// :( had to remove final from the fields, because of readElement
	
	private Line l;

	private Circle c;

	private double mul;

	/**
	 * Only used internally for reading and writing!
	 */
	public LineCircleIntersectionPoint() {}
	
	LineCircleIntersectionPoint(Line l, Circle c, double mul) {
		assert mul == -1 || mul == 1;

		this.l = l;
		this.c = c;
		this.mul = mul;
	}

	public Pos getPos() throws ImaginaryPointException {
		Pos p1 = l.getP1().getPos();
		Pos p2 = l.getP2().getPos();
		Pos cc = c.getCenter().getPos();
		Pos cf = c.getDistFrom().getPos();
		Pos ct = c.getDistTo().getPos();

		// transform line relative to center of circle
		p1 = p1.subtract(cc);
		p2 = p2.subtract(cc);

		// formule van
		// http://mathworld.wolfram.com/Circle-LineIntersection.html

		double r = cf.distance(ct);

		Pos d = p2.subtract(p1);
		double dr = d.size();
		double dm = p1.x * p2.y - p2.x * p1.y;

		double c = dr * dr;
		double disc = r * r * dr * dr - dm * dm; // discriminant

		if (disc < 0 || c == 0) {
			// eigenlijk een ongeldige lijn, ofzo, maar goed, dat levert ook
			// geen punt op
			throw new ImaginaryPointException();
		}

		double discR = Math.sqrt(disc);

		double sgn = sgn(d.y);
		double mul = this.mul * sgn;
		
		double a = dm * d.y;
		double b = sgn * d.x * discR;

		double x = (a + mul * b) / c;

		a = -dm * d.x;
		b = Math.abs(d.y) * discR;

		double y = (a + mul * b) / c;

		return new Pos(x, y).add(cc);
	}

	private static double sgn(double v) {
		// Math.signum, met één uitzondering:
		// signum(0) == 0
		// sgn(0) == -1
		if (v < 0) {
			return -1;
		} else {
			return 1;
		}
	}
	
	public void writeElement(ElementWriter out) throws IOException {
		out.writeRef(l);
		out.writeRef(c);
		out.writeDouble(mul);
	}
	
	public void readElement(ElementReader in) throws IOException {
		l = (Line) in.readRef();
		c = (Circle) in.readRef();
		mul = in.readDouble();
	}
	
	public void collectAssumptions(Collection<AssumedPoint> list) {
		l.collectAssumptions(list);
		c.collectAssumptions(list);
	}
	
	public Point copyElement() {
		return new LineCircleIntersectionPoint(l.copyElement(),
				c.copyElement(), mul);
	}
	
}