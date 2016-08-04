package states;

public class Action {
	String name;
	Goal contributesTo;
	private boolean accepted;
	
	public Action(String name)
	{
		this.name = name;
		accepted = false;
	}
	
	public String getName()
	{
		return name;
	}

	public Goal getContributesTo() {
		return contributesTo;
	}

	public void setContributesTo(Goal contributesTo) {
		this.contributesTo = contributesTo;
	}

	public boolean isAccepted() {
		return accepted;
	}

	public void setAccepted(boolean accepted) {
		this.accepted = accepted;
	}

	
	
	
}
