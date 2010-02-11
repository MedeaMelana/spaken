package spaken.model.elements;

import spaken.model.Element;

public interface Points extends Element {
	public int getPointCount();
	public Point getPoint(int index);
}
