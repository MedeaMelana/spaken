/* Created on Jun 20, 2008. */
package spaken.model.elements;

import spaken.model.*;

public interface Point extends Element<Point> {

	/**
	 * @return The position of this <tt>Point</tt>.
	 */
	public Pos getPos() throws ImaginaryPointException;

}
