package spaken.model;

import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;

/**
 * An object of the <tt>Pos</tt> class represents a fixed position in space.
 */
public class Pos {
	public static final Pos ZERO = new Pos(0, 0);

	public final double x, y;

	public Pos(double x, double y) {
		this.x = x;
		this.y = y;
	}

	public Pos(Point2D p) {
		this(p.getX(), p.getY());
	}

	public Point2D toPoint2D() {
		return new Point2D.Double(x, y);
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}

	public Pos setX(double x) {
		return new Pos(x, y);
	}

	public Pos setY(double y) {
		return new Pos(x, y);
	}

	public Pos getPos() {
		return this;
	}

	public static Pos add(Pos a, Pos b) {
		return new Pos(a.x + b.x, a.y + b.y);
	}

	public Pos add(Pos that) {
		return add(this, that);
	}

	public static Pos subtract(Pos a, Pos b) {
		return new Pos(a.x - b.x, a.y - b.y);
	}

	public Pos subtract(Pos that) {
		return subtract(this, that);
	}

	public static double distanceSquared(Pos a, Pos b) {
		return subtract(a, b).sizeSquared();
	}

	public double distanceSquared(Pos that) {
		return distanceSquared(this, that);
	}

	public static double distance(Pos a, Pos b) {
		return subtract(a, b).size();
	}

	public double distance(Pos that) {
		return distance(this, that);
	}

	public double sizeSquared() {
		return x * x + y * y;
	}

	public double size() {
		return Math.sqrt(sizeSquared());
	}

	public Pos scale(double s) {
		return new Pos(x * s, y * s);
	}

	public Pos normalise() throws NullVectorException {
		double size = size();
		if (size == 0) {
			throw new NullVectorException();
		} else {
			return new Pos(x / size, y / size);
		}
	}

	public Pos perpendicular() {
		return new Pos(y, -x);
	}

	public Pos transform(AffineTransform xf) {
		Point2D p2 = toPoint2D();
		xf.transform(p2, p2);
		return new Pos(p2);
	}

	public Pos inverseTransform(AffineTransform xf) {
		try {
			Point2D p2 = toPoint2D();
			xf.inverseTransform(p2, p2);
			return new Pos(p2);
		} catch (NoninvertibleTransformException e) {
			throw new IllegalStateException(e);
		}
	}

	public String toString() {
		return "(" + x + "," + y + ")";
	}

}
