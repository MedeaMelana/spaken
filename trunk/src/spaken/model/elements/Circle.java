/* Created on Jun 20, 2008. */
package spaken.model.elements;

import java.io.IOException;
import java.util.List;

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
	
	public Point[] getDependencies() {
		return new Point[] {center, distFrom, distTo};
	}

	public Circle makePluggableCopy(List<PluggablePoint> collect) {
		Point cc = center.makePluggableCopy(collect);
		Point dfc = distFrom.makePluggableCopy(collect);
		Point dtc = distTo.makePluggableCopy(collect);
		
		return new Circle(cc, dfc, dtc);
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
