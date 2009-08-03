package spaken.model.elements;

import spaken.model.*;

public interface Point extends Element<Point> {

	public enum Type {
		FIXED, DERIVED;
	}
	
	/**
	 * @return The position of this <tt>Point</tt>, or <tt>null</tt> if the
	 *         position is not defined.
	 */
	public Pos getPos();
	
	public Type getType();

}
