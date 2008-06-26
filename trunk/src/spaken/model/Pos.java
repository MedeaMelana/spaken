package spaken.model;

/**
 * An object of the <tt>Pos</tt> class represents a fixed position in space.
 */
public class Pos {
	public final double x, y;

	public Pos(double x, double y) {
		this.x = x;
		this.y = y;
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

	public String toString() {
		return "(" + x + "," + y + ")";
	}

}
