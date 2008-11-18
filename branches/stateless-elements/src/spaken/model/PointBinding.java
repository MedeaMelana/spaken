package spaken.model;

import spaken.model.elements.Point;

public interface PointBinding {
	public Point getPoint(int i) throws UnboundPointException;
}
