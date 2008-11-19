/* Created on Jun 20, 2008. */
package spaken.model.elements;

import java.io.IOException;
import java.util.Set;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedCircle;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Circle implements Element<Circle> {

	private Point center;

	private Point distFrom;

	private Point distTo;

	/**
	 * Only used internally for reading and writing!
	 */
	public Circle() {
	}

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

	public Rendered render(PointBinding<Pos> binding)
			throws ImaginaryPointException, UnboundPointException {
		return new RenderedCircle(center.getPos(binding), distTo
				.getPos(binding).distance(distFrom.getPos(binding)));
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		center.collectAssumptions(collect);
		distFrom.collectAssumptions(collect);
		distTo.collectAssumptions(collect);
	}

	public Circle instantiate(PointBinding<Point> binding)
			throws UnboundPointException {
		return new Circle(center.instantiate(binding), distFrom
				.instantiate(binding), distTo.instantiate(binding));
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeRef(center);
		out.writeRef(distFrom);
		out.writeRef(distTo);
	}

	public void readElement(ElementReader in) throws IOException {
		center = (Point) in.readRef();
		distFrom = (Point) in.readRef();
		distTo = (Point) in.readRef();
	}

}
