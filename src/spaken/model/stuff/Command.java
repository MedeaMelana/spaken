package spaken.model;

public interface Command {

	public void execute();

	public void undo();

	public void redo();

	public String getLabel();

}
