package spaken.model;

import java.util.List;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

public abstract class AbstractPoint implements Point {

	public Rendered render() throws ImaginaryPointException {
		return new RenderedPoint(getPos(), getRenderedPointType());
	}
	
	protected RenderedPoint.Type getRenderedPointType() {
		return RenderedPoint.Type.DERIVED;
	}

	public void makePluggable(List<PluggablePoint> collect) {
		collect.add(new PluggablePoint(this));
	}
}
