package spaken.model.elements;

import spaken.model.*;

public interface Point extends Element<Point> {

	public enum Type {
		FIXED, DERIVED;
	}

	/**
	 * @return The position of this <tt>Point</tt>. May not return
	 *         <tt>null</tt>!
	 * @throws ImaginaryPointException
	 *             if the position is not defined.
	 */
	public Pos getPos() throws ImaginaryPointException;

	public Type getType();

}
