package spaken.model.elements;

import java.util.List;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

public class PluggablePoint implements Point {
	private Point plugged;

	public PluggablePoint(Point plugged) {
		this.plugged = plugged;
	}

	public Point getPluggedPoint() {
		return plugged;
	}

	public void setPluggedPoint(Point p) {
		plugged = p;
	}

	public Pos getPos() throws ImaginaryPointException {
		return plugged.getPos();
	}

	public Rendered render() throws ImaginaryPointException {
		// TODO eigenlijk plugged.render() doen en daar iets mee doen, of
		// meerdere Rendereds opleveren.
		return new RenderedPoint(getPos(), RenderedPoint.Type.PLUGGABLE);
	}

	public Element[] getDependencies() {
		return plugged.getDependencies();
	}

	public Point makePluggableCopy(List<PluggablePoint> collect) {
		// This is never a newly generated PluggablePoint, so we should not
		// collect it.
		// TODO: possibly concatenate chains of nested PluggablePoints
		// (optimisation)
		Point cPlugged = plugged.makePluggableCopy(collect);
		PluggablePoint p = new PluggablePoint(cPlugged);
		return p;
	}
}
