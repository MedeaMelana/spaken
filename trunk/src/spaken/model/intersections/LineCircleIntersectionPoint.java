package spaken.model.intersections;

import spaken.model.*;

class LineCircleIntersectionPoint extends DerivedPoint {
	private final Line l;

	private final Circle c;

	private final double mul;

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

		double a = dm * d.y;
		double b = sgn(d.y) * d.x * discR;

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
}