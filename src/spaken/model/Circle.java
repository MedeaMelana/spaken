/* Created on Jun 20, 2008. */
package spaken.model;

public class Circle implements Element {

	private Point center;
	private Point distFrom;
	private Point distTo;

	public Circle(Point center, Point distFrom, Point distTo) {
		this.center = center;
		this.distFrom = distFrom;
		this.distTo = distTo;
	}

	public Point getCenter() {
		return center;
	}

	public Point getDistFrom() {
		return distFrom;
	}

	public Point getDistTo() {
		return distTo;
	}

}
