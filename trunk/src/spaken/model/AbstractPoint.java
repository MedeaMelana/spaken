package spaken.model;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

public abstract class AbstractPoint implements Point {

	public Rendered render() throws ImaginaryPointException {
		return new RenderedPoint(getPos(), getRenderedPointType());
	}
	
	protected RenderedPoint.Type getRenderedPointType() {
		return RenderedPoint.Type.DERIVED;
	}
}
