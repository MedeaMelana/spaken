package spaken.model.elements;

import spaken.model.*;

public interface Point extends Element {

	public Pos getPos() throws ImaginaryPointException;

}
