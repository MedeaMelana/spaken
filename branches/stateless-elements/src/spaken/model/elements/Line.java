/* Created on Jun 20, 2008. */
package spaken.model.elements;

import java.io.IOException;
import java.util.Set;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedLine;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Line implements Element<Line> {

	private Point p1;

	private Point p2;

	/**
	 * Only used internally for reading and writing!
	 */
	public Line() {
	}

	public Line(Point p1, Point p2) {
		if (p1 == p2) {
			throw new IllegalArgumentException(
					"Don't create line from two equal points.");
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

	public Rendered render(PointBinding binding)
			throws ImaginaryPointException, UnboundPointException {
		return new RenderedLine(p1.getPos(binding), p2.getPos(binding));
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		p1.collectAssumptions(collect);
		p2.collectAssumptions(collect);
	}

	public Line instantiate(PointBinding binding) throws UnboundPointException {
		return new Line(p1.instantiate(binding), p2.instantiate(binding));
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
