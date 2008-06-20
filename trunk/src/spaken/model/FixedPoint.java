/* Created on Jun 20, 2008. */
package spaken.model;

/**
 * @author Martijn van Steenbergen
 */
public class FixedPoint extends Construction {

	private double x;
	private double y;

	public FixedPoint(double x, double y) {
		this.x = x;
		this.y = y;
	}

	public double getX() {
		return x;
	}

	public void setX(double x) {
		this.x = x;
	}

	public double getY() {
		return y;
	}

	public void setY(double y) {
		this.y = y;
	}

	public void execute(Space space) {
		// TODO Auto-generated method stub

	}

}