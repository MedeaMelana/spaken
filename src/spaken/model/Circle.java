/* Created on Jun 20, 2008. */
package spaken.model;

public class Circle {

	private Point distFrom;
	private Point distTo;
	private Point center;

	public Circle(Point distFrom, Point distTo, Point center) {
		this.distFrom = distFrom;
		this.distTo = distTo;
		this.center = center;
	}

	public Point getDistFrom() {
		return distFrom;
	}

	public Point getDistTo() {
		return distTo;
	}

	public Point getCenter() {
		return center;
	}

}
