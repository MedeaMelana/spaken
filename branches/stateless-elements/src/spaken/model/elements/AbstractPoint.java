package spaken.model.elements;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

public abstract class AbstractPoint implements Point {

	public Rendered render(PointBinding binding)
			throws ImaginaryPointException, UnboundPointException {
		return new RenderedPoint(getPos(binding), getRenderedPointType());
	}

	protected RenderedPoint.Type getRenderedPointType() {
		return RenderedPoint.Type.DERIVED;
	}
}
