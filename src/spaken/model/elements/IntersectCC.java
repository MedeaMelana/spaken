package spaken.model.elements;

import java.awt.Color;
import java.awt.Graphics2D;

import spaken.model.*;

public class IntersectCC extends AbstractElement implements Points {
	private final Circle c1, c2;

	private final Point point1, point2;

	IntersectCC(Circle c1, Circle c2) {
		this.c1 = c1;
		this.c2 = c2;
		this.point1 = new IPoint(0, -1.0);
		this.point2 = new IPoint(1, 1.0);
	}

	public Pos getPointPos(double mul) throws ImaginaryPointException {
		assert mul == -1 || mul == 1;

		// http://mathworld.wolfram.com/Circle-CircleIntersection.html

		Pos p1 = c1.getCenter().getPos();
		Pos p2 = c2.getCenter().getPos();

		double r1s = c1.getDistFrom().getPos().distanceSquared(
				c1.getDistTo().getPos());
		double r2s = c2.getDistFrom().getPos().distanceSquared(
				c2.getDistTo().getPos());
		double r1 = Math.sqrt(r1s);
		double r2 = Math.sqrt(r2s);

		Pos dist = p2.subtract(p1);
		double d = dist.size();

		double xt = (d * d - r2s + r1s) / (2 * d);

		double yt = Math.sqrt((-d + r2 - r1) * (-d - r2 + r1) * (-d + r2 + r1)
				* (d + r2 + r1))
				/ d / 2;

		try {
			Pos lens = p1.add(dist.normalise().scale(xt));
			Pos inter = lens.add(dist.perpendicular().normalise().scale(
					mul * yt));
			return inter;
		} catch (NullVectorException e) {
			throw new ImaginaryPointException();
		}
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
			Elem e = IntersectCC.this.visit(sp);
			return sp.getPoint(e, i);
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
		Elem e1 = c1.visit(sp);
		Elem e2 = c2.visit(sp);
		return sp.intersectCC(e1, e2);
	}

	public void draw(Graphics2D g, double pixelSize, Color color)
			throws ImaginaryPointException {
	}

}