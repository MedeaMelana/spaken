package spaken.model.elements.intersections;

import java.util.Set;

import spaken.model.*;
import spaken.model.elements.*;

public class CircleCircleIntersectionPoint extends AbstractPoint implements Dependency<Circle> {
	// :( had to remove final from the fields, because of readElement

	private Circle c1, c2;

	private double mul;

	/**
	 * Only used internally for reading and writing!
	 */
	public CircleCircleIntersectionPoint() {
	}

	CircleCircleIntersectionPoint(Circle c1, Circle c2, double mul) {
		assert mul == -1 || mul == 1;

		this.c1 = c1;
		this.c2 = c2;
		this.mul = mul;
		
		c1.addDependency(this);
		c2.addDependency(this);
	}

	public Pos getPos() {
		// TODO uitrekenen van uitkomst in elementChanged doen.

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
			return null;
		}
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		c1.collectAssumptions(collect);
		c2.collectAssumptions(collect);
	}

	public void elementChanged(Circle e) {
		// TODO uitrekenen van uitkomst getPos hier doen.
	}

//	public Point instantiate(PointBinding binding) throws UnboundPointException {
//		return new CircleCircleIntersectionPoint(c1.instantiate(binding), c2
//				.instantiate(binding), mul);
//	}
//
//	public void writeElement(ElementWriter out) throws IOException {
//		out.writeRef(c1);
//		out.writeRef(c2);
//		out.writeDouble(mul);
//	}
//
//	public void readElement(ElementReader in) throws IOException {
//		c1 = (Circle) in.readRef();
//		c2 = (Circle) in.readRef();
//		mul = in.readDouble();
//	}

}