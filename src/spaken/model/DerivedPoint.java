package spaken.model;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

public abstract class DerivedPoint implements Point {

	public Rendered render() throws ImaginaryPointException {
		return new RenderedPoint(getPos(), true);
	}

}
