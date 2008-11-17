package spaken.storage;

import java.io.IOException;
import java.util.List;

import spaken.model.Element;
import spaken.model.Pos;

public interface ElementReader {
	// TODO some kind of ParseError instead of IOException
	
	public Element readRef() throws IOException;
	public List<Element<?>> readRefs() throws IOException;

	public Pos readPos() throws IOException;
	
	public double readDouble() throws IOException;
	public boolean readBoolean() throws IOException;
}
