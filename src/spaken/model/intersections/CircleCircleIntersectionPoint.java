package spaken.model.intersections;

import java.util.List;

import spaken.model.*;

public class CircleCircleIntersectionPoint extends AbstractPoint {
	private final Circle c1, c2;

	private final double mul;

	CircleCircleIntersectionPoint(Circle c1, Circle c2, double mul) {
		assert mul == -1 || mul == 1;

		this.c1 = c1;
		this.c2 = c2;
		this.mul = mul;
	}

	public Pos getPos() throws ImaginaryPointException {
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

		double yt = Math.sqrt((-d + r2 - r1) * (-d - r2 + r1)
				* (-d + r2 + r1) * (d + r2 + r1))
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
	
	public Element[] getDependencies() {
		return new Element[] {c1, c2};
	}
	
	public Point makePluggableCopy(List<PluggablePoint> collect) {
		Circle c1c = c1.makePluggableCopy(collect);
		Circle c2c = c2.makePluggableCopy(collect);
		
		return new CircleCircleIntersectionPoint(c1c, c2c, mul);
	}
}