/* Created on Jun 20, 2008. */
package spaken.model.elements;

import java.io.IOException;
import java.util.Collection;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedCircle;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Circle implements Element<Circle> {

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

	public Rendered render() throws ImaginaryPointException {
		return new RenderedCircle(center.getPos(), distTo.getPos().distance(
				distFrom.getPos()));
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
	
	public void collectAssumptions(Collection<AssumedPoint> list) {
		center.collectAssumptions(list);
		distFrom.collectAssumptions(list);
		distTo.collectAssumptions(list);
	}
	
	public Circle copyElement() {
		return new Circle(center, distFrom, distTo);
	}
	
}
