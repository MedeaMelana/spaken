package spaken.model.elements;

import java.awt.Color;
import java.awt.Graphics2D;

import spaken.model.*;

class IntersectLC extends AbstractElement implements Points {
	private final Line line;

	private final Circle circle;

	private final IPoint point1, point2;

	IntersectLC(Line l, Circle c) {
		this.line = l;
		this.circle = c;
		this.point1 = new IPoint(0, -1.0);
		this.point2 = new IPoint(1, 1.0);
	}

	public Pos getPointPos(double mul) throws ImaginaryPointException {
		assert mul == -1 || mul == 2;

		Pos p1 = line.getP1().getPos();
		Pos p2 = line.getP2().getPos();
		Pos cc = circle.getCenter().getPos();
		Pos cf = circle.getDistFrom().getPos();
		Pos ct = circle.getDistTo().getPos();

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
		mul = mul * sgn;

		double a = dm * d.y;
		double b = sgn * d.x * discR;

		double x = (a + mul * b) / c;

		a = -dm * d.x;
		b = Math.abs(d.y) * discR;

		double y = (a + mul * b) / c;

		return new Pos(x, y).add(cc);
	}

	private class IPoint extends AbstractPoint {
		private final double mul;

		private final int i;

		private IPoint(int i, double mul) {
			this.i = i;
			this.mul = mul;
		}

		public Pos getPos() throws ImaginaryPointException {
			return getPointPos(mul);
		}

		public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp)
				throws Err {
			Elem e = IntersectLC.this.visit(sp);
			return sp.getPoint(e, i);
		}

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

	public Point getPoint(int index) {
		if (index == 0) {
			return point1;
		} else if (index == 1) {
			return point2;
		} else {
			throw new PointIndexOutOfRangeException(index);
		}
	}

	public int getPointCount() {
		return 2;
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp)
			throws Err {
		Elem el = line.visit(sp);
		Elem ec = circle.visit(sp);
		return sp.intersectLC(el, ec);
	}

	public void draw(Graphics2D g, double pixelSize, Color color)
			throws ImaginaryPointException {
	}

}