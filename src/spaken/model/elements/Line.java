/* Created on Jun 20, 2008. */
package spaken.model.elements;

import java.io.IOException;
import java.util.List;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedLine;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Line implements Element<Line> {

	private Point p1;
	private Point p2;

	public Line(Point p1, Point p2) {
		if (p1 == p2) {
			throw new IllegalArgumentException("Don't create line from two equal points.");
		}
		this.p1 = p1;
		this.p2 = p2;
	}

	public Point getP1() {
		return p1;
	}

	public Point getP2() {
		return p2;
	}

	public Rendered render() throws ImaginaryPointException {
		return new RenderedLine(p1.getPos(), p2.getPos());
	}
	
	public Point[] getDependencies() {
		return new Point[] {p1, p2};
	}
	
	public Line makePluggableCopy(List<PluggablePoint> collect) {
		Point cp1 = p1.makePluggableCopy(collect);
		Point cp2 = p2.makePluggableCopy(collect);
		
		return new Line(cp1, cp2);
	}
	
	public void writeElement(ElementWriter out) throws IOException {
		out.writeRef(p1);
		out.writeRef(p2);
	}
	
	public void readElement(ElementReader in) throws IOException {
		p1 = (Point) in.readRef();
		p2 = (Point) in.readRef();
	}
}
