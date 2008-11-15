package spaken.model.elements.intersections;

import java.io.IOException;

import spaken.model.*;
import spaken.model.elements.*;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class CircleCircleIntersectionPoint extends AbstractPoint {
	// :( had to remove final from the fields, because of readElement

	private Circle c1, c2;

	private double mul;

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
	
	public void writeElement(ElementWriter out) throws IOException {
		out.writeRef(c1);
		out.writeRef(c2);
		out.writeDouble(mul);
	}
	
	public void readElement(ElementReader in) throws IOException {
		c1 = (Circle) in.readRef();
		c2 = (Circle) in.readRef();
		mul = in.readDouble();
	}
}