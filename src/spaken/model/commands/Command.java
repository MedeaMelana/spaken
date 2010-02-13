package spaken.model.commands;

public interface Command {

	public void execute();

	public void undo();

	public void redo();

	public String getLabel();

}
